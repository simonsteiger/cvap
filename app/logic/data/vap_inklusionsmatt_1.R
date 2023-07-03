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
    dp$filter(tidig_ra == 1) %>% # QUESTION is this correct if we also want early SPA, PSA, AS?
    dp$mutate(
        alder = lub$interval(fodelsedag, inkluderad) / lub$dyears(1),
        min_inkl_diag = pmin(diagnosdatum1, inkluderad, na.rm = TRUE),
        lan = ifelse(lan == "Örebro", "Orebro", lan)
    )

# Visit groups need to be dynamically calculated in this VAP
# Variable diff
# Variable start_var

fst$write_fst(out, "app/logic/data/vap_inklusionsmatt_1.fst")
