box::use(
    magrittr[`%>%`],
    fst,
    dp = dplyr,
    lub = lubridate,
    pr = purrr,
)

box::use(
    ski = swissknife / skinit,
    srqlib / srqprep,
    srqlib / srqdate,
    srqlib / srqdict,
)

ski$read_dir("/Users/simonsteiger/Desktop/data/fst/")

bas <- list_df$basdata %>%
    srqprep$prep_recode(diagnoskod_1, srqdict$rec_dxcat, .new_name = dxcat) %>%
    dp$filter(tidig_ra == 1) %>% # TODO likely need to flexibly determine who is early dxcat
    dp$mutate(
        alder = lub$interval(fodelsedag, inkluderad) / lub$dyears(1),
        min_inkl_diag = pmin(diagnosdatum1, inkluderad, na.rm = TRUE),
        lan = ifelse(lan == "Örebro", "Orebro", lan)
    ) %>%
    dp$left_join(list_df$terapi, by = "patientkod", suffix = c("", ".dupl")) %>%
    dp$select(patientkod, lan, kon, dxcat, prep_typ, ordinerat, pagaende, insatt, utsatt, min_inkl_diag, inkluderad, diagnosdatum1)
# FIX many-to-many join

out <- list_df$besoksdata %>%
    dp$select(patientkod, datum) %>%
    dp$inner_join(bas, by = "patientkod")

unit_seq <- seq(from = 4, to = 12, by = 2)
unit_min <- lub$dmonths(2) / lub$ddays(1)
start_vars <- c("min_inkl_diag", "diagnosdatum1", "inkluderad")

res <- pr$map(start_vars, \(start) {
    pr$map2(unit_seq, start, \(t, s) {
        t_days <- lub$dmonths(t) / lub$ddays(1)
        out %>%
            dp$mutate(diff = as.numeric(datum - .data[[s]])) %>%
            dp$filter(.data[[s]] <= lub$today() - t_days) %>%
            dp$mutate(
                visit_group = ifelse(diff <= t_days & diff >= unit_min, TRUE, FALSE),
                timestamp = factor(t),
                start = factor(s)
            ) %>%
            dp$arrange(patientkod, dp$desc(visit_group)) %>%
            dp$distinct(patientkod, .keep_all = TRUE)
    }) %>%
        pr$list_rbind()
}) %>%
    pr$list_rbind()

fst$write_fst(res, "app/logic/data/vap_inklusionsmatt_1.fst")
