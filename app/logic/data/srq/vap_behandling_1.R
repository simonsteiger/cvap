box::use(
    fst,
    magrittr[`%>%`],
    dp = dplyr,
    ts = tidyselect,
    lub = lubridate,
    fct = forcats,
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
    dp$mutate(lan_scb_id = as.numeric(lan_scb_id) * -1) # reverse order here due to coord_flip later

# Join SCB län IDs onto basdata and prepare dxcat
bas_lan <- list_df$basdata %>%
    dp$left_join(lan_coding, by = dp$join_by(lan == lan_no_suffix)) %>%
    srqprep$prep_recode(diagnoskod_1, srqdict$rec_dxcat, .new_name = dxcat) %>%
    dp$mutate(
        lan = ifelse(lan == "Örebro", "Orebro", lan), # dirty fix for names in geo table
        dxcat = ifelse(!dxcat %in% c("RA", "AS", "SpA", "PsA"), "Annan", dxcat),
        dxcat = as.factor(dxcat)
    )

# Join terapi data onto basdata
out <- bas_lan %>%
    dp$left_join(list_df$terapi, by = "patientkod", suffix = c("", ".dupl")) %>%
    ada$set_utsatt() %>%
    dp$select(-ts$contains(".dupl")) %>%
    dp$mutate(alder = lub$interval(fodelsedag, ordinerat) / lub$dyears(1)) %>%
    dp$filter(alder >= 18, prep_typ == "bioprep") %>% # QUESTION should the filters in the app take care?
    dp$select(patientkod, lan, lan_scb_id, kon, dxcat, prep_typ, ordinerat, pagaende, utsatt)

fst$write_fst(out, "app/logic/data/srq/clean/vap_behandling_1.fst")
