box::use(
    fst,
    magrittr[`%>%`],
    lub = lubridate,
    dp = dplyr,
    pr = purrr,
    ts = tidyselect,
    rl = rlang,
    tdr = tidyr,
    str = stringr,
)

box::use(
    ski = app / logic / swissknife / skinit, # dl data from server in deployment
    app / logic / srqlib / srqdict,
    app / logic / srqlib / srqprep,
    local = app / logic / data / PATH,
)

ski$read_dir(local$PATH)

lan_coding <- dp$select(list_df$lan_coding, lan_no_suffix, lan_scb_id) %>%
    dp$mutate(lan_scb_id = as.numeric(lan_scb_id) * -1) # reverse bc coord_flip in bar

basdata <- list_df$basdata %>%
    dp$left_join(lan_coding, dp$join_by(lan == lan_no_suffix)) %>%
    srqprep$prep_recode(diagnoskod_1, srqdict$rec_dxcat, .new_name = dxcat) %>%
    dp$filter(dxcat == "RA") %>%
    dp$mutate(lan = ifelse(lan == "Örebro", "Orebro", lan)) %>%
    dp$select(patientkod, fodelsedag, dxcat, lan, lan_scb_id, tillhor)

besoksdata <- list_df$besoksdata %>%
    dp$select(patientkod, datum, das28, cdai, besoks_id)

bio <- list_df$bio %>%
    dp$arrange(patientkod, ordinerat) %>%
    dp$distinct(patientkod, .keep_all = TRUE) %>%
    dp$mutate(
        prep_start = pmax(ordinerat, insatt, na.rm = TRUE),
        preparat = ifelse(preparat == "Roactemra", "RoActemra", preparat)
    )

bas_bio <-
    dp$inner_join(
        bio, basdata,
        by = "patientkod",
        suffix = c("", ".dupl")
    ) %>%
    dp$select(-ts$contains(c(".dupl", "skapad", "andrad")), -c("preparat_kod", "orsak", "ar"))

out <-
    dp$left_join(bas_bio, besoksdata, by = "patientkod") %>%
    dp$mutate(
        alder = lub$interval(fodelsedag, ordinerat) / lub$dyears(1),
        diff = as.numeric(datum - prep_start),
        visit_group = factor(dp$case_when(
            diff >= -30 & diff <= 7 ~ "Behandlingsstart",
            diff >= 120 & diff <= 365 ~ "Uppföljning",
            .default = NA
        )),
        # This is the opposite of the result to allow correct sorting in the double map below
        das28_high = ifelse(das28 <= 3.2, FALSE, TRUE),
        cdai_high = ifelse(cdai <= 10, FALSE, TRUE),
    ) %>%
    dp$filter(!is.na(visit_group)) %>%
    tdr$nest(.by = visit_group) %>%
    dp$mutate(
        # keep obs depending on visit_group, see conds in visit_group mutate comments above
        data = list(
            pr$map(c("das28_high", "cdai_high"), \(outer) {
                pr$map2(data, c("diff", outer), \(df, inner) {
                    df %>%
                        dp$mutate(outcome = outer, iteration = inner) %>%
                        # This arrange statement sorts the shortest diff to top and also the lowest (FALSE) boolean value
                        # Hence the unintuitive flip above
                        dp$arrange(dp$across(ts$all_of(c("patientkod", inner)))) %>%
                        dp$distinct(.data[["patientkod"]], .keep_all = TRUE)
                }) %>%
                    pr$list_rbind()
            }) %>%
                pr$list_rbind()
        ) # returns an inflated version of what we need, FIX!
    ) %>%
    tdr$unnest(data) %>%
    # Flip booleans and outcome string
    dp$mutate(
        das28_low = !das28_high,
        cdai_low = !cdai_high,
        outcome = str$str_replace(outcome, "high", "low"),
    ) %>%
    dp$filter( # cleaning out what is not needed later
        (visit_group == "Behandlingsstart" & iteration == "diff") |
            (visit_group == "Uppföljning" & iteration != "diff")
    ) %>%
    dp$distinct(patientkod, visit_group, outcome, .keep_all = TRUE) %>%
    dp$mutate(dummy_outcome = outcome) %>%
    tdr$nest(.by = dummy_outcome) %>%
    dp$mutate(
        data = pr$map(data, \(df) {
            if (unique(df$outcome) == "das28_high") {
                dp$filter(df, !is.na(das28_high))
            } else {
                dp$filter(df, !is.na(cdai_high))
            }
        })
    ) %>%
    tdr$unnest(cols=c(data)) %>%
    dp$select(-c(ts$contains("_high"), dummy_outcome, id, tillhor, fodelsedag, iteration, insatt, utsatt, pagaende))

fst$write_fst(out, "app/logic/data/srq/clean/vap_indikatorer_4.fst")

# OLD PIPELINE DOES NOT SELECT NEAREST VISIT TO DIAGNOSIS FOR BEHANDLINGSSTART
# OLD PIPELINE DOES NOT SELECT FIRST BIO
