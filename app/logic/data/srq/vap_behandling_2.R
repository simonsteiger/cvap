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

bas_ter <- list_df$basdata %>%
    srqprep$prep_recode(diagnoskod_1, srqdict$rec_dxcat, .new_name = dxcat) %>%
    dp$mutate(
        lan = ifelse(lan == "Örebro", "Orebro", lan)
    ) %>%
    dp$left_join(list_df$terapi, by = "patientkod", suffix = c("", ".dupl")) %>%
    dp$select(-ts$contains(".dupl")) %>%
    ada$set_utsatt() %>%
    dp$mutate(
        prep_start = pmax(ordinerat, insatt, na.rm = TRUE), # QUESTION prep_start vs ordinerat?
        alder = lub$interval(fodelsedag, ordinerat) / lub$dyears(1),
        dxcat = ifelse(!dxcat %in% c("RA", "AS", "SpA", "PsA"), "Annan", dxcat),
        dxcat = factor(dxcat, c("RA", "AS", "SpA", "PsA", "Annan"))
        ) %>%
    # QUESTION should the age filters in the app take care?
    dp$filter(
        alder >= 18,
        prep_typ == "bioprep"
    ) %>%
    dp$select(patientkod, lan, kon, dxcat, ordinerat, pagaende, utsatt)

pop <-
    dp$left_join(
        list_df$pop,
        list_df$lan_coding,
        by = c("Region_ID" = "lan_scb_id")
    ) %>%
    dp$filter(
        Region_ID != "00",
        Age >= 18,
        Year == max(Year),
        Sex %in% c("Male", "Female")
        # most precise results would demand calculating patients / visit per THAT year's population
        # but error is likely so marginal that using the report year should be OK for now
    ) %>%
    dp$summarise(
        lan_scb_id = as.numeric(Region_ID) * -1, # reverse because of coord_flip in barplot
        population = sum(Population),
        .by = c(Year, lan_no_suffix) # keep lan_scb_id
    ) %>%
    dp$select(lan = lan_no_suffix, lan_scb_id, Year, population)
# recreate Riket later by summarising, can't be matched before

simplified_pop <-
    pop %>%
    dp$distinct() %>%
    dp$mutate(lan = ifelse(lan == "Örebro", "Orebro", lan)) %>%
    dp$filter(Year == max(Year)) %>%
    dp$select(-Year)

out <- dp$left_join(bas_ter, simplified_pop, by = "lan")

fst$write_fst(out, "app/logic/data/srq/clean/vap_behandling_2.fst")
