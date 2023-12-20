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
    dp$filter(dxcat == "RA") %>%
    dp$mutate(lan = ifelse(lan == "Örebro", "Orebro", lan)) %>%
    dp$select(patientkod, fodelsedag, dxcat, lan, lan_scb_id, tillhor)

list_df$besoksdata <- list_df$besoksdata %>%
    dp$select(patientkod, datum, das28, cdai, besoks_id)

list_df$terapi <- list_df$terapi %>%
    dp$filter(prep_typ == "bioprep") %>%
    dp$arrange(patientkod, ordinerat) %>%
    dp$distinct(patientkod, .keep_all = TRUE) %>%
    dp$mutate(
        prep_start = pmax(ordinerat, insatt, na.rm = TRUE),
        preparat = ifelse(preparat == "Roactemra", "RoActemra", preparat)
    )

list_df$bas_ter <-
    dp$inner_join(
        list_df$terapi, list_df$basdata,
        by = "patientkod",
        suffix = c("", ".dupl")
    ) %>%
    dp$select(-ts$contains(c(".dupl", "skapad", "andrad")), -c("preparat_kod", "orsak", "ar"))

out <-
    dp$left_join(list_df$bas_ter, list_df$besoksdata, by = "patientkod") %>%
    dp$mutate(
        alder = lub$interval(fodelsedag, ordinerat) / lub$dyears(1),
        diff = as.numeric(datum - prep_start),
        visit_group = factor(dp$case_when(
            diff >= -30 & diff <= 7 ~ "Behandlingsstart",
            diff >= 120 & diff <= 365 ~ "Uppföljning",
            .default = NA
        )),
        das28_low = ifelse(das28 <= 3.2, TRUE, FALSE),
        cdai_low = ifelse(cdai <= 10, TRUE, FALSE),
    ) %>%
    dp$filter(!is.na(visit_group)) %>%
    tdr$nest(.by = visit_group) %>%
    dp$mutate(
        # keep obs depending on visit_group, see conds in visit_group mutate comments above
        data = list(
            pr$map(c("das28_low", "cdai_low"), \(outer) {
                pr$map2(data, c("diff", outer), \(df, inner) {
                    df %>%
                        dp$mutate(outcome = outer, iteration = inner) %>%
                        dp$arrange(dp$across(ts$all_of(c("patientkod", inner)))) %>%
                        dp$distinct(.data[["patientkod"]], .keep_all = TRUE)
                }) %>%
                    pr$list_rbind()
            }) %>%
                pr$list_rbind()
        ) # returns an inflated version of what we need, FIX!
    ) %>%
    tdr$unnest(data) %>%
    dp$filter( # cleaning out what is not needed later
        (visit_group == "Behandlingsstart" & iteration == "diff") |
            (visit_group == "Uppföljning" & iteration != "diff")
    ) %>%
    dp$distinct(patientkod, visit_group, outcome, .keep_all = TRUE) %>%
    dp$mutate(dummy_outcome = outcome) %>%
    tdr$nest(.by = dummy_outcome) %>%
    dp$mutate(
        data = pr$map(data, \(df) {
            if (unique(df$outcome) == "das28_low") {
                dp$filter(df, !is.na(das28_low))
            } else {
                dp$filter(df, !is.na(cdai_low))
            }
        })
    ) %>%
    tdr$unnest(cols=c(data)) %>%
    dp$select(-c(dummy_outcome, id, tillhor, fodelsedag, iteration, insatt, utsatt, pagaende))

fst$write_fst(out, "app/logic/data/srq/clean/vap_indikatorer_4.fst")

#### OLD PIPELINE DOES NOT SELECT NEAREST VISIT TO DIAGNOSIS FOR BEHANDLINGSSTART

# Checked patients where this is true are 204 and 425

ski$read_dir(local$PATH)

behand <- list_df$terapi

besoksdata <- list_df$besoksdata

basdata2 <- list_df$basdata %>%
  dp$distinct(patientkod, lan, fodelsedag,enhet,pal,diagnoskod_1,kon)

RA<-c("M05.3","M05.8L","M05.8M","M05.8N","M05.9","M05.9L","M05.9M","M05.9N",
      "M06.0","M06.0L","M06.0M","M06.0N","M06.8L","M06.8M","M06.8N","M06.9",
      "M12.3" )
SPA<-c("M46.8","M46.9","M08.1")
PSA<-c("M07.0","M07.1","M07.2","M07.3x+L40.5","M09.0",
       "M07.0+L40.5","M07.1+L40.5","M07.2+L40.5")
AS<-c("M45.9")

basdata2$diagnos<-ifelse(basdata2$diagnoskod_1%in%RA,"RA",
                         ifelse(basdata2$diagnoskod_1%in%SPA,"SPA",
                                ifelse(basdata2$diagnoskod_1%in%PSA,"PSA",
                                       ifelse(basdata2$diagnoskod_1%in%AS,"AS","Annan"))))

behand$prep_typ<-ifelse(behand$prep_typ=="dmard" | behand$prep_typ=="csdmard" | behand$prep_typ=="tsdmard",
                            "dmard",
                            behand$prep_typ)
################ Sorterar behandlings-data efter patientkod och ordineringsdatum #######################

data1 <- behand %>%
  dp$group_by(patientkod, ordinerat)

data <- data1 %>%
  dp$filter(prep_typ=="dmard" | prep_typ=="bioprep") %>%
  dp$distinct(patientkod, ordinerat,insatt,utsatt,prep_typ,kon,pagaende)

bio3 <- data %>%
  dp$group_by(patientkod,prep_typ) %>%
  dp$filter(ordinerat == min(ordinerat)) %>%
  dp$mutate(bio_start = max(ordinerat,insatt, na.rm=T)) %>%
  dp$mutate(ar = lub$year(bio_start)) %>%
  dp$ungroup() %>%
  dp$distinct(patientkod,
           ar,
           bio_start,
           kon,
           prep_typ)

t1 <- dp$inner_join(bio3, basdata2, by="patientkod") %>%
  dp$mutate(age_start = floor((bio_start-as.Date(fodelsedag))/365.25)) %>%
  dp$filter(18 <= age_start & age_start <= 100) %>%
  dp$rename(kon=kon.x)


data1 <- dp$left_join(t1,dp$select(besoksdata, besoks_id, patientkod, datum, das28, besoks_id), by="patientkod") %>%
  dp$filter(prep_typ=="bioprep") %>%
  dp$filter(diagnos=="RA") %>%
  dp$filter(!is.na(das28)) %>%
  dp$mutate(follow_up_time = datum - bio_start) %>%
  dp$mutate(besokstyp = ifelse(120 <= follow_up_time & follow_up_time <= 365,"Uppföljning",0)) %>%
  dp$filter(besokstyp=="Uppföljning") %>%
  dp$mutate(matt="Das28") %>%
  # filter(120 <= follow_up_time & follow_up_time <= 365) %>%
  dp$group_by(patientkod) %>%
  dp$filter(das28 == min(das28)) %>%
  dp$mutate(low=ifelse(das28<=3.2,1,0)) %>%
  dp$ungroup() %>%
  dp$distinct(patientkod,
           kon,
           ar,
           low,
           lan,
           age_start,
           besokstyp,
           matt,
           bio_start,
           enhet,
           pal, .keep_all=TRUE)


data2 <- dp$left_join(t1,dp$select(besoksdata, besoks_id, patientkod, datum, das28, besoks_id), by="patientkod") %>%
  dp$filter(prep_typ=="bioprep") %>%
  dp$filter(diagnos=="RA") %>%
  dp$filter(!is.na(das28)) %>%
  dp$mutate(follow_up_time = datum - bio_start) %>%
  dp$mutate(besokstyp = ifelse(-30 <= follow_up_time & follow_up_time <= 7,"Behandlingsstart",0)) %>%
  dp$filter(besokstyp=="Behandlingsstart") %>%
  dp$mutate(matt="Das28") %>%
  # filter(120 <= follow_up_time & follow_up_time <= 365) %>%
  dp$group_by(patientkod) %>%
  dp$filter(das28 == min(das28)) %>%
  dp$mutate(low=ifelse(das28<=3.2,1,0)) %>%
  dp$ungroup() %>%
  dp$distinct(patientkod,
           kon,
           ar,
           low,
           lan,
           age_start,
           besokstyp,
           matt,
           bio_start,
           enhet,
           pal, .keep_all=TRUE)


data2<-dp$bind_rows(data1,data2)

data3 <- dp$left_join(t1,dp$select(besoksdata, besoks_id, patientkod, datum, cdai), by="patientkod") %>%
  dp$filter(prep_typ=="bioprep") %>%
  dp$filter(diagnos=="RA") %>%
  dp$filter(!is.na(cdai)) %>%
  dp$mutate(follow_up_time = datum - bio_start) %>%
  dp$mutate(besokstyp = ifelse(120 <= follow_up_time & follow_up_time <= 365,"Uppföljning",0)) %>%
  dp$filter(besokstyp=="Uppföljning") %>%
  dp$mutate(matt="cdai") %>%
  # filter(120 <= follow_up_time & follow_up_time <= 365) %>%
  dp$group_by(patientkod) %>%
  dp$filter(cdai == min(cdai)) %>%
  dp$mutate(low=ifelse(cdai<=10,1,0)) %>%
  dp$ungroup() %>%
  dp$distinct(patientkod,
           kon,
           ar,
           low,
           lan,
           age_start,
           besokstyp,
           matt,
           bio_start,
           enhet,
           pal, .keep_all=TRUE)


data4 <- dp$left_join(t1,dp$select(besoksdata, besoks_id, patientkod, datum, cdai), by="patientkod") %>%
  dp$filter(prep_typ=="bioprep") %>%
  dp$filter(diagnos=="RA") %>%
  dp$filter(!is.na(cdai)) %>%
  dp$mutate(follow_up_time = datum - bio_start) %>%
  dp$mutate(besokstyp = ifelse(-30 <= follow_up_time & follow_up_time <= 7,"Behandlingsstart",0)) %>%
  dp$filter(besokstyp=="Behandlingsstart") %>%
  dp$mutate(matt="cdai") %>%
  # filter(120 <= follow_up_time & follow_up_time <= 365) %>%
  dp$group_by(patientkod) %>%
  dp$filter(cdai == min(cdai)) %>%
  dp$mutate(low=ifelse(cdai<=10,1,0)) %>%
  dp$ungroup() %>%
  dp$distinct(patientkod,
           kon,
           ar,
           low,
           lan,
           age_start,
           besokstyp,
           matt,
           bio_start,
           enhet,
           pal, .keep_all=TRUE)


data5<-dp$bind_rows(data3,data4)


data2<-dp$bind_rows(data2,data5)
