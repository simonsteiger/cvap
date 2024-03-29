box::use(
    fst,
    magrittr[`%>%`],
    lub = lubridate,
    dp = dplyr,
    pr = purrr,
    ts = tidyselect,
    rl = rlang,
    tdr = tidyr,
    here,
    dotenv,
)

box::use(
    ski = app / logic / swissknife / skinit,
    app / logic / srqlib / srqdict,
    app / logic / srqlib / srqprep,
    ada = app / logic / data / aux_data,
)

dotenv$load_dot_env(file = here$here(".env"))

ski$read_dir(Sys.getenv("PATH"))

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

terapi <- list_df$terapi %>%
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
                    # IMPORTANT make sure that Behandlingsstart is first
                    # so that low abs_diff is sorted to top there
                    # and Uppföljning is second so that low `outer` is sorted to top there
                    pr$map2(data, c("abs_diff", outer), \(df, inner) {
                        df %>%
                            dp$mutate(
                                outcome = outer,
                                iteration = inner,
                                # arrange sorts non-na values to top, then lowest `inner`
                                isna = is.na(.[[outer]])
                            ) %>%
                            dp$group_by(prep_typ) %>%
                            dp$arrange(dp$across(ts$all_of(c("patientkod", "isna", inner)))) %>%
                            dp$distinct(.data[["patientkod"]], .keep_all = TRUE) %>%
                            dp$ungroup()
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
    dp$distinct(patientkod, visit_group, outcome, prep_typ, .keep_all = TRUE) %>%
    dp$select(-c(id, tillhor, diagnos_1, diagnos_2, atc_kod, fodelsedag))

fst$write_fst(out, "app/logic/data/srq/clean/vap_behandling_4.fst")
