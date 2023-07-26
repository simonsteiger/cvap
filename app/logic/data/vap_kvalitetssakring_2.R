box::use(
    fst,
    tdr = tidyr,
    dp = dplyr,
    ts = tidyselect,
    str = stringr,
    fct = forcats,
    lub = lubridate,
    magrittr[`%>%`],
)

box::use(
    ski = swissknife / skinit, # dl data from server in deployment
)

# here goes data base download later
ski$read_dir("/Users/simonsteiger/Desktop/data/fst/")

coverage <- list_df$coverage %>%
    dp$mutate(
        dp$across(ts$contains("20"), \(x) as.numeric(x))
    ) %>%
    tdr$pivot_longer(!c(Lan, Kod), names_to = "year", values_to = "outcome") %>%
    dp$rename(lan = "Lan") %>%
    dp$mutate(
        lan = ifelse(lan == "Örebro", "Orebro", lan),
        year = lub$ymd(paste(str$str_remove(year, "Y_"), "12", "31", sep = "-")),
        outcome = ifelse(is.na(outcome), 0, outcome / 100)
        # we want percent as decimals at this point
    ) %>%
    dp$select(-Kod)

fst$write_fst(coverage, "app/logic/data/vap_kvalitetssakring_2.fst")
