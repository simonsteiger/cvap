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
)

# here goes data base download later
ski$read_dir("/Users/simonsteiger/Desktop/data/fst/")

# the data needs to go through the qrdf preprocessing, too

list_df$basdata <- list_df$basdata %>%
    srqprep$prep_recode(diagnoskod_1, srqdict$rec_dxcat, .new_name = dxcat) %>%
    dp$filter(dxcat == "RA") %>%
    dp$mutate(lan = ifelse(lan == "Örebro", "Orebro", lan)) %>%
    dp$select(patientkod, fodelsedag, dxcat, lan, tillhor)

list_df$besoksdata <- list_df$besoksdata %>%
    dp$select(patientkod, datum, das28, haq, patientens_globala) %>%
    dp$mutate(patientens_globala = as.numeric(patientens_globala)) # unparsable strings become NA

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
    dp$select(-ts$contains(c(".dupl", "skapad", "andrad")), -c("preparat_kod", "orsak", "ar"))

out <-
    dp$left_join(list_df$bas_bio, list_df$besoksdata, by = "patientkod") %>%
    dp$mutate(
        alder = lub$interval(fodelsedag, datum) / lub$dyears(1),
        diff = as.numeric(datum - prep_start),
        visit_group = factor(dp$case_when(
            diff >= -30 & diff <= 7 ~ "Behandlingsstart",
            diff >= 120 & diff <= 365 ~ "Uppföljning",
            .default = NA
        ))
    ) %>%
    dp$filter(!is.na(visit_group))

fst$write_fst(out, "app/logic/data/vap_indikatorer_3.fst")
