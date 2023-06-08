box::use(
    magrittr[`%>%`],
    fst,
    dp = dplyr,
    lub = lubridate,
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
        min_inkl_diag = pmin(diagnosdatum1, inkluderad, na.rm = TRUE)
    ) %>%
    srqprep$prep_dynamic_groups(
        .start = srqdate$no_limit,
        .end = lub$today(),
        .start_var = "min_inkl_diag",
        .end_var = "symtomdebut_1",
        diff >= -140 ~ TRUE,
        .default = FALSE
    )

fst$write_fst(out, "app/logic/data/vap_behandling_1.fst")
