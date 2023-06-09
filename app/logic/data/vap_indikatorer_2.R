box::use(
    magrittr[`%>%`],
    fst,
    dp = dplyr,
    lub = lubridate,
    ts = tidyselect,
)

box::use(
    ski = swissknife / skinit,
    srqlib / srqprep,
    srqlib / srqdate,
    srqlib / srqdict,
)

ski$read_dir("/Users/simonsteiger/Desktop/data/fst/")

bas_ter <- list_df$basdata %>%
    srqprep$prep_recode(diagnoskod_1, srqdict$rec_dxcat, .new_name = dxcat) %>%
    dp$mutate(lan = ifelse(lan == "Örebro", "Orebro", lan)) %>%
    dp$left_join(list_df$terapi, by = "patientkod", suffix = c("", ".dupl")) %>%
    dp$select(-ts$contains(".dupl")) %>%
    dp$mutate(alder = lub$interval(fodelsedag, ordinerat) / lub$dyears(1)) %>%
    dp$filter(
        alder >= 18,
        dxcat == "RA"
    ) %>%
    dp$select(patientkod, lan, kon, inkluderad, ordinerat, pagaende, utsatt)

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
        population = sum(Population),
        .by = c(Year, lan_no_suffix)
    ) %>%
    dp$select(lan = lan_no_suffix, Year, population)
# recreate Riket later by summarising, can't be matched before

simplified_pop <-
    pop %>%
    dp$mutate(lan = ifelse(lan == "Örebro", "Orebro", lan)) %>%
    dp$filter(Year == max(Year)) %>%
    dp$select(-Year)

out <- dp$left_join(bas_ter, simplified_pop, by = "lan")

fst$write_fst(out, "app/logic/data/vap_indikatorer_2.fst")
