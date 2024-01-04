box::use(
    fst,
    magrittr[`%>%`],
    lub = lubridate,
    dp = dplyr,
    pr = purrr,
    ts = tidyselect,
    rl = rlang,
    tdr = tidyr,
)

box::use(
    ski = app / logic / swissknife / skinit,
    app / logic / srqlib / srqdict,
    app / logic / srqlib / srqprep,
    ada = app / logic / data / aux_data,
    local = app / logic / data / PATH,
)

ski$read_dir(local$PATH)

lan_coding <- dp$select(list_df$lan_coding, lan_no_suffix, lan_scb_id) %>%
    dp$mutate(lan_scb_id = as.numeric(lan_scb_id) * -1) # reverse bc coord_flip in bar

bas_lan <- list_df$basdata %>%
    dp$left_join(lan_coding, by = dp$join_by(lan == lan_no_suffix)) %>%
    srqprep$prep_recode(diagnoskod_1, srqdict$rec_dxcat, .new_name = dxcat) %>%
    dp$mutate(lan = ifelse(lan == "Örebro", "Orebro", lan)) %>%
    dp$select(patientkod, fodelsedag, dxcat, lan, lan_scb_id, tillhor, avslutad)

besoksdata <- list_df$besoksdata %>%
    dp$select(patientkod, datum, smarta, haq, patientens_globala) %>%
    dp$mutate(
        dp$across(ts$all_of(c("patientens_globala", "smarta")), \(x) as.numeric(as.character(x)))
    ) # unparsable strings become NA

# Get prep_typ into list_df$bio
# It seems we should not work with bio, but with terapi - need csdmard
# list_df$bio <- list_df$terapi %>%
#     dp$select(preparat_kod, prep_typ) %>%
#     dp$distinct() %>%
#     dp$right_join(list_df$bio, by = "preparat_kod") %>%
#     dp$filter(!is.na(prep_typ))

terapi <- list_df$terapi %>% # TODO Why are we full joining terapi and bio???
    dp$full_join(list_df$bio, by = "patientkod", suffix = c("", ".dupl")) %>%
    dp$select(-ts$contains(".dupl")) %>%
    dp$filter(prep_typ %in% c("bioprep", "csdmard")) %>%
    dp$arrange(patientkod, ordinerat) %>%
    dp$distinct(patientkod, prep_typ, .keep_all = TRUE) %>%
    dp$mutate(
        prep_start = pmax(ordinerat, insatt, na.rm = TRUE),
        preparat = ifelse(preparat == "Roactemra", "RoActemra", preparat)
    )

bas_lan_terapi <-
    dp$inner_join(
        terapi, bas_lan,
        by = "patientkod",
        suffix = c("", ".dupl")
    ) %>%
    dp$select(-ts$contains(c(".dupl", "skapad", "andrad")), -c("preparat_kod", "orsak", "ar")) %>%
    ada$set_utsatt()

out <-
    dp$left_join(bas_lan_terapi, besoksdata, by = "patientkod") %>%
    dp$mutate(
        alder = lub$interval(fodelsedag, datum) / lub$dyears(1),
        diff = abs(as.numeric(datum - prep_start)),
        visit_group = factor(dp$case_when(
            diff >= -30 & diff <= 7 ~ "Behandlingsstart", # use diff closest to 0
            diff >= 120 & diff <= 365 ~ "Uppföljning", # use lowest target value (p_glob)
            .default = NA
        )),
        abs_diff = abs(diff), # we want closest diff to 0, not most negative
    ) %>%
    dp$filter(!is.na(visit_group)) %>%
    tdr$nest(.by = visit_group) %>%
    dp$mutate(
        # keep obs depending on visit_group, see conds in visit_group mutate comments above
        data = list(
            pr$map(
                c("patientens_globala", "haq", "smarta"), \(outer) {
                    pr$map2(data, c(outer, "abs_diff"), \(df, inner) {
                        df %>%
                            # don't sort missings to top in diff iteration
                            dp$filter(!is.na(.[[outer]])) %>%
                            dp$mutate(outcome = outer, iteration = inner) %>%
                            dp$arrange(dp$across(ts$all_of(c("patientkod", inner)))) %>%
                            dp$distinct(.data[["patientkod"]], .keep_all = TRUE)
                    }) %>%
                        pr$list_rbind()
                }
            ) %>%
                pr$list_rbind()
        ) # returns an inflated version of what we need, FIX!
    ) %>%
    tdr$unnest(data) %>%
    dp$filter( # cleaning out what is not needed later
        (visit_group == "Behandlingsstart" & iteration == "abs_diff") |
            (visit_group == "Uppföljning" & iteration != "abs_diff")
    ) %>%
    dp$distinct(patientkod, visit_group, outcome, .keep_all = TRUE) %>%
    dp$select(-c(id, tillhor, diagnos_1, diagnos_2, atc_kod, fodelsedag, iteration))

fst$write_fst(out, "app/logic/data/srq/clean/vap_behandling_4.fst")
