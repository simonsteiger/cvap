box::use(
    fst,
    magrittr[`%>%`],
    lub = lubridate,
    dp = dplyr,
    pr = purrr,
    ts = tidyselect,
    rl = rlang,
)

box::use(
    ski = app / logic / swissknife / skinit,
    app / logic / srqlib / srqdict,
    app / logic / srqlib / srqprep,
    ada = app / logic / data / aux_data,
    local = app / logic / data / PATH,
)

ski$read_dir(local$PATH)

lan_coding <- dp$select(list_df$lan_coding, lan_no_suffix, lan_scb_id) %>%
    dp$mutate(lan_scb_id = as.numeric(lan_scb_id) * -1) # reverse bc coord_flip in bar

bas_lan <- list_df$basdata %>%
    dp$left_join(lan_coding, by = dp$join_by(lan == lan_no_suffix)) %>%
    srqprep$prep_recode(diagnoskod_1, srqdict$rec_dxcat, .new_name = dxcat) %>%
    dp$filter(dxcat == "RA") %>%
    dp$mutate(lan = ifelse(lan == "Örebro", "Orebro", lan)) %>%
    dp$select(patientkod, fodelsedag, dxcat, lan, lan_scb_id, tillhor, avslutad)

besoksdata <-  dp$select(list_df$besoksdata, patientkod, datum, das28, cdai)

bio <- list_df$bio %>%
    dp$arrange(patientkod, ordinerat) %>%
    dp$distinct(patientkod, .keep_all = TRUE) %>%
    dp$mutate(
        prep_start = pmax(ordinerat, insatt, na.rm = TRUE), # QUESTION use prep_start?
        preparat = ifelse(preparat == "Roactemra", "RoActemra", preparat)
    )

bas_lan_bio <-
    dp$inner_join(
        bio, bas_lan,
        by = "patientkod",
        suffix = c("", ".dupl")
    ) %>%
    dp$select(-ts$contains(c(".dupl", "skapad", "andrad")), -c("preparat_kod", "orsak", "ar")) %>%
    ada$set_utsatt()

out <-
    dp$left_join(bas_lan_bio, besoksdata, by = "patientkod") %>%
    dp$mutate(
        alder = lub$interval(fodelsedag, datum) / lub$dyears(1),
        das28_low = ifelse(das28 < 3.2, TRUE, FALSE),
        cdai_low = ifelse(cdai <= 10, TRUE, FALSE)
    ) %>%
    dp$arrange(patientkod, das28) %>%
    dp$select(-c(id, tillhor, fodelsedag))

fst$write_fst(out, "app/logic/data/srq/clean/vap_behandling_3.fst")
