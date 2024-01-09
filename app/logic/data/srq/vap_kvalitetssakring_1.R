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
    ski = app / logic / swissknife / skinit,
    app / logic / srqlib / srqdict,
    app / logic / srqlib / srqprep,
    local = app / logic / data / PATH,
)

ski$read_dir(local$PATH)

lan_coding <- dp$select(list_df$lan_coding, lan_no_suffix, lan_scb_id) %>%
    dp$mutate(lan_scb_id = as.numeric(lan_scb_id) * -1) # reverse bc coord_flip in bar

# the data needs to go through the qrdf preprocessing, too

list_df$basdata <- list_df$basdata %>%
    dp$left_join(lan_coding, dp$join_by(lan == lan_no_suffix)) %>%
    srqprep$prep_recode(diagnoskod_1, srqdict$rec_dxcat, .new_name = dxcat) %>%
    dp$mutate(
        lan = ifelse(lan == "Örebro", "Orebro", lan),
        dxcat = ifelse(!dxcat %in% c("RA", "AS", "SpA", "PsA"), "Annan", dxcat),
        dxcat = factor(dxcat, c("RA", "AS", "SpA", "PsA", "Annan"))
    ) %>%
    dp$select(patientkod, fodelsedag, dxcat, lan, lan_scb_id, tillhor)

list_df$besoksdata <- list_df$besoksdata %>%
    dp$select(patientkod, datum)

# had bio here previously, but we need data on csdmards, too

bas_ter <- list_df$terapi %>%
    dp$mutate(
        min_ins_ord = pmin(ordinerat, insatt, na.rm = TRUE),
        preparat = ifelse(preparat == "Roactemra", "RoActemra", preparat)
    ) %>%
    dp$left_join(list_df$basdata, by = "patientkod", suffix = c("", ".dupl")) %>%
    dp$select(-ts$contains(".dupl")) %>%
    dp$filter(prep_typ %in% c("bioprep", "csdmard")) %>%
    dp$arrange(patientkod, ordinerat) %>%
    dp$distinct(patientkod, prep_typ, .keep_all = TRUE) %>%
    srqprep$prep_line() %>%
    dp$filter(line == 1)

bas_ter_bes <-
    dp$left_join(bas_ter, list_df$besoksdata, by = "patientkod") %>%
    dp$mutate(
        alder = lub$interval(fodelsedag, datum) / lub$dyears(1),
        diff = as.numeric(datum - min_ins_ord)
    )

unit_seq <- seq(from = 4, to = 12, by = 2)
unit_min <- lub$dmonths(2) / lub$ddays(1)

# QUESTION currently working with fixed min_ins_ord - correct?
out <- pr$map(unit_seq, \(t) {
    t_days <- lub$dmonths(t) / lub$ddays(1)
    bas_ter_bes %>%
        # Keep only those where ordination is at least as long ago as today - <current_limit>
        dp$filter(min_ins_ord <= lub$today() - t_days) %>%
        dp$group_by("prep_typ") %>%
        dp$mutate(
            visit_group = ifelse(diff <= t_days & diff >= unit_min, TRUE, FALSE),
            timestamp = t
            # this was factor(t) – but why would we want that to be a factor here?
            # we need it to be a factor in inklusionsmatt, but not here
        ) %>%
        dp$arrange(patientkod, dp$desc(visit_group)) %>%
        dp$distinct(patientkod, .keep_all = TRUE) %>%
        dp$ungroup()
    # If a patient has a visit in the current timeframe, sort it to top and remove rest
}) %>%
    pr$list_rbind() %>%
    dp$select(c(patientkod, kon, prep_typ, ordinerat, dxcat, lan, lan_scb_id, datum, alder, visit_group, timestamp))

fst$write_fst(out, "app/logic/data/srq/clean/vap_kvalitetssakring_1.fst")
