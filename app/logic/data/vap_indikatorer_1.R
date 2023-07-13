box::use(
    magrittr[`%>%`],
    fst,
    dp = dplyr,
    lub = lubridate,
    pr = purrr,
)

box::use(
    ski = swissknife / skinit,
    srqlib / srqprep,
    srqlib / srqdate,
)

ski$read_dir("/Users/simonsteiger/Desktop/data/fst/")

out <- list_df$basdata %>%
    dp$filter(tidig_ra == 1) %>% # also check for dxcat == "RA"?
    dp$mutate(
        alder = lub$interval(fodelsedag, inkluderad) / lub$dyears(1),
        min_inkl_diag = pmin(diagnosdatum1, inkluderad, na.rm = TRUE),
        lan = ifelse(lan == "Örebro", "Orebro", lan)
    )

out <- pr$map(c("min_inkl_diag", "diagnosdatum1", "inkluderad"), \(v) {
    out %>%
        dp$mutate(start = v) %>%
        srqprep$prep_dynamic_groups(
            .start = srqdate$no_limit,
            .end = lub$today(),
            .start_var = "symtomdebut_1",
            .end_var = v,
            diff <= 140 & diff >= 0 ~ TRUE,
            .default = FALSE
        )
}) %>%
    pr$list_rbind()

fst$write_fst(out, "app/logic/data/vap_indikatorer_1.fst")
