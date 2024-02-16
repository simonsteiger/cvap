box::use(
    fst,
    tdr = tidyr,
    dp = dplyr,
    ts = tidyselect,
    str = stringr,
    fct = forcats,
    lub = lubridate,
    magrittr[`%>%`],
    here,
    dotenv,
)

box::use(
    ski = app / logic / swissknife / skinit,
)

dotenv$load_dot_env(file = here$here(".env"))

ski$read_dir(Sys.getenv("PATH"))

lan_coding <- dp$select(list_df$lan_coding, lan_no_suffix, lan_scb_id) %>%
    dp$mutate(lan_scb_id = as.numeric(lan_scb_id) * -1) # reverse bc coord_flip in bar

coverage <- list_df$coverage %>%
    dp$mutate(
        dp$across(ts$contains("20"), \(x) as.numeric(x))
    ) %>%
    tdr$pivot_longer(!c(Lan, Kod), names_to = "year", values_to = "outcome") %>%
    dp$rename(lan = "Lan") %>%
    dp$mutate(
        lan = dp$case_when(
            lan == "Örebro" ~ "Orebro",
            lan == "SWEDEN" ~ "Riket",
            .default = lan
        ),
        year = lub$ymd(paste(str$str_remove(year, "Y_"), "12", "31", sep = "-")),
        outcome = ifelse(is.na(outcome), 0, outcome / 100)
        # we want percent as decimals at this point
    ) %>%
    dp$left_join(lan_coding, dp$join_by(lan == lan_no_suffix)) %>% # join after SWEDEN renamed Riket
    dp$select(-Kod)

fst$write_fst(coverage, "app/logic/data/srq/clean/vap_kvalitetssakring_2.fst")
