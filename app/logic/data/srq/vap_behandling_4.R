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
)

# here goes data base download later
ski$read_dir("/Users/simonsteiger/Desktop/data/fst/")

list_df$basdata <- list_df$basdata %>%
    srqprep$prep_recode(diagnoskod_1, srqdict$rec_dxcat, .new_name = dxcat) %>%
    dp$mutate(lan = ifelse(lan == "Örebro", "Orebro", lan)) %>%
    dp$select(patientkod, fodelsedag, dxcat, lan, tillhor, avslutad)

list_df$besoksdata <- list_df$besoksdata %>%
    dp$select(patientkod, datum, smarta, haq, patientens_globala) %>%
    dp$mutate(
        dp$across(ts$all_of(c("patientens_globala", "smarta")), \(x) as.numeric(x))
    ) # unparsable strings become NA

# Get prep_typ into list_df$bio
# It seems we should not work with bio, but with terapi - need csdmard
# list_df$bio <- list_df$terapi %>%
#     dp$select(preparat_kod, prep_typ) %>%
#     dp$distinct() %>%
#     dp$right_join(list_df$bio, by = "preparat_kod") %>%
#     dp$filter(!is.na(prep_typ))

list_df$terapi <- list_df$terapi %>%
    dp$full_join(list_df$bio, by = "patientkod", suffix = c("", ".dupl")) %>%
    dp$select(-ts$contains(".dupl")) %>%
    dp$filter(prep_typ %in% c("bioprep", "csdmard")) %>%
    dp$arrange(patientkod, ordinerat) %>%
    dp$distinct(patientkod, prep_typ, .keep_all = TRUE) %>%
    dp$mutate(
        prep_start = pmax(ordinerat, insatt, na.rm = TRUE),
        preparat = ifelse(preparat == "Roactemra", "RoActemra", preparat)
    )

list_df$bas_terapi <-
    dp$inner_join(
        list_df$terapi, list_df$basdata,
        by = "patientkod",
        suffix = c("", ".dupl")
    ) %>%
    dp$select(-ts$contains(c(".dupl", "skapad", "andrad")), -c("preparat_kod", "orsak", "ar")) %>%
    ada$set_utsatt()

out <-
    dp$left_join(list_df$bas_terapi, list_df$besoksdata, by = "patientkod") %>%
    dp$mutate(
        alder = lub$interval(fodelsedag, datum) / lub$dyears(1),
        diff = abs(as.numeric(datum - prep_start)),
        visit_group = factor(dp$case_when(
            diff >= -30 & diff <= 7 ~ "Behandlingsstart", # use diff closest to 0
            diff >= 120 & diff <= 365 ~ "Uppföljning", # use lowest target value (p_glob)
            .default = NA
        ))
    ) %>%
    dp$filter(!is.na(visit_group)) %>%
    tdr$nest(.by = visit_group) %>%
    dp$mutate(
        # keep obs depending on visit_group, see conds in visit_group mutate comments above
        data = list(
            pr$map(
                c("patientens_globala", "haq", "smarta"), \(outer) {
                    pr$map2(
                        .x = data,
                        .y = c(outer, "diff"),
                        .f = \(df, inner) {
                            df %>%
                                dp$mutate(outcome = outer, iteration = inner) %>%
                                dp$arrange(dp$across(ts$all_of(c("patientkod", inner)))) %>%
                                dp$distinct(.data[["patientkod"]], .keep_all = TRUE)
                        }
                    ) %>%
                        pr$list_rbind()
                }
            ) %>%
                pr$list_rbind()
        ) # returns an inflated version of what we need, FIX!
    ) %>%
    tdr$unnest(data) %>%
    dp$filter( # cleaning out what is not needed later
        (visit_group == "Behandlingsstart" & iteration == "diff") |
            (visit_group == "Uppföljning" & iteration != "diff")
    ) %>%
    dp$distinct(patientkod, visit_group, outcome, .keep_all = TRUE)

fst$write_fst(out, "app/logic/data/srq/vap_behandling_4.fst")
