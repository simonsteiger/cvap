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
    ski = app / logic / swissknife / skinit, # dl data from server in deployment
    app / logic / srqlib / srqdict, # include in project for deployment?
    app / logic / srqlib / srqprep, # include in project for deployment?
    app / logic / srqlib / srqdate, # include in project for deployment?
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
        min_ins_ord = pmin(ordinerat, insatt, na.rm = TRUE),
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
    dp$mutate(alder = lub$interval(fodelsedag, datum) / lub$dyears(1))

unit_seq <- seq(from = 4, to = 12, by = 2)
unit_min <- lub$dmonths(2) / lub$ddays(1)

# TODO currently working with fixed min_ins_ord - correct?
res <- pr$map(unit_seq, \(t) {
    t_days <- lub$dmonths(t) / lub$ddays(1)
    out %>%
        dp$mutate(diff = as.numeric(datum - min_ins_ord)) %>%
        dp$filter(min_ins_ord <= lub$today() - t_days) %>%
        dp$mutate(
            visit_group = ifelse(diff <= t_days & diff >= unit_min, TRUE, FALSE),
            timestamp = factor(t)
        ) %>%
        dp$arrange(patientkod, dp$desc(visit_group)) %>%
        dp$distinct(patientkod, .keep_all = TRUE)
}) %>%
    pr$list_rbind()



fst$write_fst(res, "app/logic/data/vap_kvalitetssakring_1.fst")
