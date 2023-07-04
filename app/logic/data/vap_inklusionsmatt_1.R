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

bas <- list_df$basdata %>%
    srqprep$prep_recode(diagnoskod_1, srqdict$rec_dxcat, .new_name = dxcat) %>%
    dp$filter(tidig_ra == 1) %>% # QUESTION is this correct if we also want early SPA, PSA, AS?
    dp$mutate(
        alder = lub$interval(fodelsedag, inkluderad) / lub$dyears(1),
        min_inkl_diag = pmin(diagnosdatum1, inkluderad, na.rm = TRUE),
        lan = ifelse(lan == "Örebro", "Orebro", lan)
    ) %>%
    dp$left_join(list_df$terapi, by = "patientkod", suffix = c("", ".dupl")) %>%
    dp$select(patientkod, lan, kon, dxcat, prep_typ, ordinerat, pagaende, insatt, utsatt, min_inkl_diag)
# FIX many-to-many join

out <- list_df$besoksdata %>%
    dp$select(patientkod, datum) %>%
    dp$inner_join(bas, by = "patientkod")

fst$write_fst(out, "app/logic/data/vap_inklusionsmatt_1.fst")
