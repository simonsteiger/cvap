box::use(
    magrittr[`%>%`],
    fst,
    dp = dplyr,
    lub = lubridate,
    pr = purrr,
)

box::use(
    ski = app / logic / swissknife / skinit,
    app / logic / srqlib / srqprep,
    app / logic / srqlib / srqdict,
    local = app / logic / data / PATH,
)

ski$read_dir(local$PATH)

# TODO chuck unnecessary variables

lan_coding <- dp$select(list_df$lan_coding, lan_no_suffix, lan_scb_id) %>%
    dp$mutate(lan_scb_id = as.numeric(lan_scb_id) * -1) # reverse bc coord_flip in bar

pre <- list_df$basdata %>%
    dp$left_join(lan_coding, by = dp$join_by(lan == lan_no_suffix)) %>%
    srqprep$prep_recode(diagnoskod_1, srqdict$rec_dxcat, .new_name = dxcat) %>%
    dp$filter(tidig_ra == 1 & dxcat == "RA") %>%
    dp$mutate(
        alder = lub$interval(fodelsedag, inkluderad) / lub$dyears(1),
        min_inkl_diag = pmin(diagnosdatum1, inkluderad, na.rm = TRUE),
        lan = ifelse(lan == "Örebro", "Orebro", lan)
    )

out <- pr$map(
    c("min_inkl_diag", "diagnosdatum1", "inkluderad"), \(t) {
        pre %>%
            dp$filter(!is.na(.data[[t]])) %>%
            dp$mutate(start = factor(t)) %>%
            srqprep$prep_dynamic_groups(
                .start = lub$ymd("1999-01-01"),
                .end = lub$today(),
                .start_var = "symtomdebut_1",
                .end_var = t,
                diff <= 140 & diff >= 0 ~ TRUE,
                .default = FALSE
            )
    }
) %>%
    pr$list_rbind() %>%
    dp$select(patientkod, inkluderad, diagnosdatum1, min_inkl_diag, symtomdebut_1, diff, kon, visit_group, lan, lan_scb_id, start, alder)


fst$write_fst(out, "app/logic/data/srq/clean/vap_indikatorer_1.fst")

# Replicate relevant part of old pipeline

# library(Epi) # don't want packrat to capture this
# uncomment when running the chunk below

out$diff2 <- cal.yr(out$inkluderad)-cal.yr(out$symtomdebut_1)
out$diff2 <- out$diff2 * 365.7 / 7
out$diff3 <- out$diff2 < 20
dp$select(out, diff, diff2, visit_group, diff3)
out$matches <- out$visit_group == out$diff3
dp$filter(out, !matches)

# Old vap allows negative differences between symptom start and diagnosis date
# Should this be the case?