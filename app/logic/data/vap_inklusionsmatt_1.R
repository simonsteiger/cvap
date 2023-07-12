box::use(
    magrittr[`%>%`],
    fst,
    dp = dplyr,
    lub = lubridate,
    pr = purrr,
    tdr = tidyr,
    tbl = tibble,
    fct = forcats,
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
    # dp$filter(tidig_ra == 1) %>% # TODO likely need to flexibly determine who is early dxcat
    dp$mutate(
        alder = lub$interval(fodelsedag, inkluderad) / lub$dyears(1),
        min_inkl_diag = pmin(diagnosdatum1, inkluderad, na.rm = TRUE),
        lan = ifelse(lan == "Örebro", "Orebro", lan)
    ) %>%
    dp$left_join(list_df$terapi, by = "patientkod", suffix = c("", ".dupl")) %>%
    dp$select(patientkod, lan, kon, dxcat, prep_typ, ordinerat, pagaende, insatt, utsatt, min_inkl_diag, inkluderad, diagnosdatum1, symtomdebut_1)
# FIX many-to-many join

out <- list_df$besoksdata %>%
    dp$select(patientkod, datum) %>%
    dp$inner_join(bas, by = "patientkod")

# OK for RA
unit_seq_ra <- seq(from = 10, to = 50, by = 10)
unit_seq_spa <- c(6, 12, 24, 48, 60)

start_vars <- c("min_inkl_diag", "diagnosdatum1", "inkluderad")

create_l <- function(data) {
    tbl$tibble(data = data, fn = list(lub$dweeks, lub$dmonths), t = list(unit_seq_ra, unit_seq_spa), s = list(start_vars))
}

# RA requires 20-60 weeks, SpA/AS/PsA 1-60 months
# Loop over nested data with category-specific values
res <- out %>%
    dp$filter(dxcat %in% c("RA", "SpA", "AS", "PsA")) %>%
    dp$mutate(ra_bool = ifelse(dxcat == "RA", TRUE, FALSE)) %>%
    tdr$nest(.by = ra_bool) %>%
    dp$mutate(
        data =
            pr$pmap(create_l(data), \(data, fn, s1, t1) {
                pr$map2(s1, list(data), \(s2, d2) {
                    pr$map2(t1, list(d2), \(t2, d3) {
                        t_days <- fn(t2) / lub$ddays(1)
                        # this is goofy / hacky, why can't I subset the data pronoun?
                        # why do we have to pass dx while min is available in deepest from outer?
                        d3 %>%
                            dp$mutate(diff = as.numeric(d3[[s2]] - symtomdebut_1)) %>%
                            dp$filter(d3[[s2]] <= lub$today() - t_days) %>%
                            dp$mutate(
                                visit_group = ifelse(diff <= t_days, TRUE, FALSE),
                                timestamp = t2,
                                start = factor(s2)
                            ) %>%
                            dp$arrange(patientkod, dp$desc(visit_group)) %>%
                            dp$distinct(patientkod, .keep_all = TRUE)
                    }) %>%
                        pr$list_rbind()
                }) %>%
                    pr$list_rbind()
            })
    ) %>%
    tdr$unnest(data) %>%
    dp$select(-c(ra_bool, diff)) %>%
    dp$mutate(dxcat = paste("Tidig", dxcat))

fst$write_fst(res, "app/logic/data/vap_inklusionsmatt_1.fst")
