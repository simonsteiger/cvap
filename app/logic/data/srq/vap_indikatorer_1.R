box::use(
    magrittr[`%>%`],
    fst,
    dp = dplyr,
    lub = lubridate,
    pr = purrr,
)

box::use(
    ski = app / logic / swissknife / skinit,
    app / logic / srqlib / srqprep,
    app / logic / srqlib / srqdict,
    local = app / logic / data / PATH,
)

ski$read_dir(local$PATH)

# TODO chuck unnecessary variables

out <- list_df$basdata %>%
srqprep$prep_recode(diagnoskod_1, srqdict$rec_dxcat, .new_name = dxcat) %>%
    dp$filter(tidig_ra == 1 & dxcat == "RA") %>% # TODO check for dxcat == "RA" correct?
    dp$mutate(
        alder = lub$interval(fodelsedag, inkluderad) / lub$dyears(1),
        min_inkl_diag = pmin(diagnosdatum1, inkluderad, na.rm = TRUE),
        lan = ifelse(lan == "Örebro", "Orebro", lan)
    )

out <- pr$map(
    c("min_inkl_diag", "diagnosdatum1", "inkluderad"), \(t) {
    out %>%
        dp$mutate(start = factor(t)) %>%
        srqprep$prep_dynamic_groups(
            .start = lub$ymd("1999-01-01"),
            .end = lub$today(),
            .start_var = "symtomdebut_1",
            .end_var = t,
            diff <= 140 & diff >= 0 ~ TRUE,
            .default = FALSE
        )
}) %>%
    pr$list_rbind() %>%
    dp$select(patientkod, inkluderad, kon, visit_group, lan, start, alder)


fst$write_fst(out, "app/logic/data/srq/clean/vap_indikatorer_1.fst")
