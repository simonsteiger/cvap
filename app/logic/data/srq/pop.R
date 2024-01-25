box::use(
    fst,
    magrittr[`%>%`],
    dp = dplyr,
    ts = tidyselect,
    lub = lubridate,
)

box::use(
    ski = app / logic / swissknife / skinit,
    ada = app / logic / data / aux_data,
    local = app / logic / data / PATH,
)

ski$read_dir(local$PATH)

pop <-
    dp$left_join(
        list_df$pop,
        list_df$lan_coding,
        by = c("Region_ID" = "lan_scb_id")
    ) %>%
    dp$filter(
        Region_ID != "00",
        Age >= 18,
        # Year == max(Year),
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
    dp$mutate(
        lan = ifelse(lan == "Örebro", "Orebro", lan),
        Year = as.numeric(Year)
    ) %>%
    dp$filter(Year >= 1995)

fst$write_fst(simplified_pop, "app/logic/data/srq/clean/pop.fst")
