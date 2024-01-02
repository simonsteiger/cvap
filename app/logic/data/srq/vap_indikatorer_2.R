box::use(
    magrittr[`%>%`],
    fst,
    dp = dplyr,
    lub = lubridate,
    ts = tidyselect,
)

box::use(
    ski = app / logic / swissknife / skinit,
    app / logic / srqlib / srqprep,
    app / logic / srqlib / srqdict,
    ada = app / logic / data / aux_data,
    local = app / logic / data / PATH,
)

ski$read_dir(local$PATH)

lan_coding <- dp$select(list_df$lan_coding, lan_no_suffix, lan_scb_id) %>%
    dp$mutate(lan_scb_id = as.numeric(lan_scb_id) * -1) # reverse bc coord_flip in bar

out <- list_df$basdata %>%
    dp$left_join(lan_coding, dp$join_by(lan == lan_no_suffix)) %>%
    srqprep$prep_recode(diagnoskod_1, srqdict$rec_dxcat, .new_name = dxcat) %>%
    dp$mutate(lan = ifelse(lan == "Örebro", "Orebro", lan)) %>%
    dp$left_join(list_df$bio, by = "patientkod", suffix = c("", ".dupl")) %>%
    dp$select(-ts$contains(".dupl")) %>%
    ada$set_utsatt() %>%
    dp$mutate(
        prep_start = pmax(ordinerat, insatt, na.rm = TRUE), # QUESTION prep_start vs ordinerat
        alder = lub$interval(fodelsedag, ordinerat) / lub$dyears(1)
    ) %>%
    dp$filter(
        dp$between(alder, 18, 100),
        dxcat == "RA",
    ) %>%
    dp$select(patientkod, lan, lan_scb_id, kon, inkluderad, ordinerat, pagaende, utsatt)

fst$write_fst(out, "app/logic/data/srq/clean/vap_indikatorer_2.fst")
