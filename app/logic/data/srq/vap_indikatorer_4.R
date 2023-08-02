box::use(
    fst,
    magrittr[`%>%`],
    lub = lubridate,
    dp = dplyr,
    pr = purrr,
    ts = tidyselect,
    rl = rlang,
    tdr = tidyr,
)

box::use(
    ski = app / logic / swissknife / skinit, # dl data from server in deployment
    app / logic / srqlib / srqdict, # include in project for deployment?
    app / logic / srqlib / srqprep, # include in project for deployment?
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
    dp$select(patientkod, datum, das28, cdai)

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
        )),
        das28_low = ifelse(das28 < 3.2, TRUE, FALSE),
        cdai_low = ifelse(cdai <= 10, TRUE, FALSE),
    ) %>%
    dp$filter(!is.na(visit_group)) %>%
    tdr$nest(.by = visit_group) %>%
    dp$mutate(
        # keep obs depending on visit_group, see conds in visit_group mutate comments above
        data = list(
            pr$map(
                c("das28_low", "cdai_low"), \(outer) {
                    pr$map2(
                        .x = data,
                        .y = c("diff", outer),
                        .f = \(df, inner) {
                            df %>%
                                dp$mutate(outcome = outer, iteration = inner) %>%
                                dp$arrange(dp$across(ts$all_of(c("patientkod", inner)))) %>%
                                dp$distinct(.data[["patientkod"]], .keep_all = TRUE)
                        }
                    ) %>%
                        pr$list_rbind()
                }
            ) %>%
                pr$list_rbind()
        ) # returns an inflated version of what we need, FIX!
    ) %>%
    tdr$unnest(data) %>%
    dp$filter( # cleaning out what is not needed later
        (visit_group == "Behandlingsstart" & iteration == "diff") |
            (visit_group == "Uppföljning" & iteration != "diff")
    ) %>%
    dp$distinct(patientkod, visit_group, outcome, .keep_all = TRUE)

fst$write_fst(out, "app/logic/data/srq/vap_indikatorer_4.fst")
