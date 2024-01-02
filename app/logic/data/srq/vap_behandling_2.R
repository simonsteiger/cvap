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

lan_coding <- dp$select(list_df$lan_coding, lan_no_suffix, lan_scb_id) %>%
    dp$mutate(lan_scb_id = as.numeric(lan_scb_id) * -1) # reverse bc coord_flip in bar

out <- list_df$basdata %>%
    srqprep$prep_recode(diagnoskod_1, srqdict$rec_dxcat, .new_name = dxcat) %>%
    dp$mutate(
        lan = ifelse(lan == "Örebro", "Orebro", lan)
    ) %>%
    dp$left_join(list_df$bio, by = "patientkod", suffix = c("", ".dupl")) %>%
    dp$select(-ts$contains(".dupl")) %>%
    ada$set_utsatt() %>%
    dp$mutate(
        prep_start = pmax(ordinerat, insatt, na.rm = TRUE), # QUESTION prep_start vs ordinerat?
        alder = lub$interval(fodelsedag, ordinerat) / lub$dyears(1),
        dxcat = ifelse(!dxcat %in% c("RA", "AS", "SpA", "PsA"), "Annan", dxcat),
        dxcat = factor(dxcat, c("RA", "AS", "SpA", "PsA", "Annan"))
    ) %>%
    # QUESTION should the age filters in the app take care?
    dp$filter(dp$between(alder, 18, 100)) %>%
    dp$select(patientkod, lan, kon, dxcat, ordinerat, pagaende, utsatt)

fst$write_fst(out, "app/logic/data/srq/clean/vap_behandling_2.fst")
