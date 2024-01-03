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
    ski = app / logic / swissknife / skinit, # dl data from server in deployment
    app / logic / srqlib / srqdict,
    app / logic / srqlib / srqprep,
    local = app / logic / data / PATH,
)

ski$read_dir(local$PATH)

lan_coding <- dp$select(list_df$lan_coding, lan_no_suffix, lan_scb_id) %>%
    dp$mutate(lan_scb_id = as.numeric(lan_scb_id) * -1) # reverse bc coord_flip in bar

list_df$basdata <- list_df$basdata %>%
    dp$left_join(lan_coding, dp$join_by(lan == lan_no_suffix)) %>%
    srqprep$prep_recode(diagnoskod_1, srqdict$rec_dxcat, .new_name = dxcat) %>%
    dp$filter(dxcat == "RA") %>%
    dp$mutate(lan = ifelse(lan == "Örebro", "Orebro", lan)) %>%
    dp$select(patientkod, fodelsedag, dxcat, lan, lan_scb_id, tillhor)

list_df$besoksdata <- list_df$besoksdata %>%
    dp$select(patientkod, datum, das28, haq, patientens_globala) %>%
    dp$mutate(patientens_globala = as.numeric(patientens_globala)) # unparsable strings become NA

list_df$bio <- list_df$bio %>%
    dp$arrange(patientkod, ordinerat) %>%
    dp$distinct(patientkod, .keep_all = TRUE) %>%
    dp$mutate(
        prep_start = pmax(ordinerat, insatt, na.rm = TRUE),
        preparat = ifelse(preparat == "Roactemra", "RoActemra", preparat)
    )

list_df$bas_bio <-
    dp$inner_join(
        list_df$bio, list_df$basdata,
        by = "patientkod",
        suffix = c("", ".dupl")
    ) %>%
    dp$select(-ts$contains(c(".dupl", "skapad", "andrad")), -c("preparat_kod", "orsak", "ar"))

out <-
    dp$left_join(list_df$bas_bio, list_df$besoksdata, by = "patientkod") %>%
    dp$mutate(
        alder = lub$interval(fodelsedag, datum) / lub$dyears(1),
        diff = as.numeric(datum - prep_start), # we had abs(...) here before, why did we do that?
        visit_group = factor(
            dp$case_when(
                diff >= -30 & diff <= 7 ~ "Behandlingsstart", # use diff closest to 0
                diff >= 120 & diff <= 365 ~ "Uppföljning", # use lowest target value (p_glob)
                .default = NA
            )
        ),
        abs_diff = abs(diff) # we want the value closest to 0, not the most negative
    ) %>%
    dp$filter(
        !is.na(visit_group),
        !is.na(patientens_globala)
    ) %>%
    tdr$nest(.by = visit_group) %>%
    dp$mutate(
        # keep obs depending on visit_group, see conds in visit_group mutate comments above
        data = pr$map2(data, c("abs_diff", "patientens_globala"), \(df, .var) {
            df %>%
                dp$arrange(dp$across(ts$all_of(c("patientkod", .var)))) %>%
                dp$distinct(patientkod, .keep_all = TRUE)
        })
    ) %>%
    tdr$unnest(data) %>%
    dp$select(-c(id, tillhor, fodelsedag))

fst$write_fst(out, "app/logic/data/srq/clean/vap_indikatorer_3.fst")
