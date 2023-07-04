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
    ski = swissknife / skinit, # dl data from server in deployment
    srqlib / srqdict, # include in project for deployment?
    srqlib / srqprep, # include in project for deployment?
    srqlib / srqdate, # include in project for deployment?
)

# here goes data base download later
ski$read_dir("/Users/simonsteiger/Desktop/data/fst/")

# the data needs to go through the qrdf preprocessing, too

list_df$basdata <- list_df$basdata %>%
    srqprep$prep_recode(diagnoskod_1, srqdict$rec_dxcat, .new_name = dxcat) %>%
    dp$mutate(lan = ifelse(lan == "Örebro", "Orebro", lan)) %>%
    dp$select(patientkod, fodelsedag, dxcat, lan, tillhor)

list_df$besoksdata <- list_df$besoksdata %>%
    dp$select(patientkod, datum)

list_df$bio <- list_df$bio %>%
    dp$arrange(patientkod, ordinerat) %>%
    dp$distinct(patientkod, .keep_all = TRUE) %>%
    dp$mutate(
        prep_start = pmax(ordinerat, insatt, na.rm = TRUE),
        preparat = ifelse(preparat == "Roactemra", "RoActemra", preparat)
    )

list_df$bas_bio <-
    dp$inner_join(
        list_df$bio, list_df$basdata,
        by = "patientkod",
        suffix = c("", ".dupl")
    ) %>%
    dp$select(-ts$contains(c(".dupl", "skapad", "andrad")), -c("preparat_kod", "orsak", "ar")) %>%
    srqprep$prep_line() %>%
    dp$filter(line == 1)

out <-
    dp$left_join(list_df$bas_bio, list_df$besoksdata, by = "patientkod") %>%
    dp$mutate(
        min_ins_ord = pmin(insatt, ordinerat, na.rm = TRUE),
        alder = lub$interval(fodelsedag, datum) / lub$dyears(1)
    ) %>%
    srqprep$prep_dynamic_groups(
        .start = srqdate$no_limit,
        .end = lub$today() - lub$dmonths(6), # ordination must be at least 6 months ago
        .start_var = "min_ins_ord",
        .end_var = "datum",
        diff >= -180 & diff <= -60 ~ TRUE,
        .default = FALSE
    ) %>%
    dp$arrange(patientkod, dp$desc(visit_group)) %>%
    dp$distinct(patientkod, .keep_all = TRUE)

fst$write_fst(out, "app/logic/data/vap_kvalitetssakring_1.fst")
