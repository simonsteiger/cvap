box::use(
    fst,
    magrittr[`%>%`],
    dp = dplyr,
    ts = tidyselect,
    lub = lubridate,
)

box::use(
    srqlib / srqdict,
    srqlib / srqprep,
    ski = app / logic / swissknife / skinit,
    ada = app / logic / data / aux_data,
    local = app / logic / data / PATH,
)

ski$read_dir(local$PATH)

out <- list_df$basdata %>%
    srqprep$prep_recode(diagnoskod_1, srqdict$rec_dxcat, .new_name = dxcat) %>%
    dp$mutate(
        lan = ifelse(lan == "Örebro", "Orebro", lan),
        dxcat = ifelse(!dxcat %in% c("RA", "AS", "SpA", "PsA"), "Annan", dxcat),
        dxcat = as.factor(dxcat)
    ) %>%
    dp$left_join(list_df$terapi, by = "patientkod", suffix = c("", ".dupl")) %>%
    ada$set_utsatt() %>%
    dp$select(-ts$contains(".dupl")) %>%
    dp$mutate(alder = lub$interval(fodelsedag, ordinerat) / lub$dyears(1)) %>%
    dp$filter(alder >= 18) %>% # QUESTION should the filters in the app take care?
    dp$select(patientkod, lan, kon, dxcat, prep_typ, ordinerat, pagaende, utsatt)

fst$write_fst(out, "app/logic/data/srq/clean/vap_behandling_1.fst")
