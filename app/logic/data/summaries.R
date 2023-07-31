box::use(
    sh = shiny,
    pr = purrr,
    str = stringr,
    magrittr[`%>%`],
)

#' @export
indikatorer_1 <-
    "Det förvalda diagrammet visar andel inkluderade individer, 18 år eller äldre vid inklusion, med tidig reumatoid artrit (<13 månader från symtomdebut/sjukdomsdebut till inklusion) som vid tidpunkt för inklusion/diagnos hade en sjukdomsduration på 20 veckor eller mindre, visat per län och för riket, för individer inkluderade under hela det föregående året.
Diagrammet motsvarar en av indikatorerna i Socialstyrelsens Öppna jämförelser. Den vertikala linjen vid 50 % representerar målvärdet, att andelen patienter med reumatoid artrit som nydiagnosticeras inom 20 veckor bör vara ≥50%.
Tidpunkten för inklusion/diagnos definieras som det tidigaste av datumen för inklusion och diagnos, eller datum för inklusion om data saknas för variabeln 'Datum för diagnos'.
Annat tidsintervall, åldersgrupp, datum för inklusion eller diagnos, data för endast män respektive kvinnor, samt om konfidensintervall eller procenttal ska visas är valbart. Länen kan sorteras efter länsnummer eller procenttal. De data som ligger till grund för diagrammet ses under fliken 'Tabell'."

#' @export
indikatorer_2 <-
    "Det förvalda diagrammet visar antalet inkluderade individer, 18 år eller äldre, med reumatoid artrit som vid tidpunkten hade en registrerad, pågående behandling med ett biologiskt läkemedel (bDMARD) per 100000 invånare, visat per län och för riket. Diagrammet motsvarar en av indikatorerna i Socialstyrelsens Öppna jämförelser. Det vertikala intervallet representerar målvärdet och motsvarar genomsnittet för riket ±25%.
Annan tidpunkt, jämförelsetidpunkt, diagnos, data för endast män respektive kvinnor, ålder hos patienterna samt om antal individer eller antal individer/100000 invånare ska visas är valbart. Länen kan sorteras efter länsnummer eller antal individer.
De data som ligger till grund för diagrammet ses under fliken 'Tabell'. De läkemedel som inkluderas i begreppen biologiska läkemedel (bDMARD) kan ses under XXX.
I undergrupper med färre än 10 individer visas inga data i diagram eller tabell.
Uppgifterna om folkmängden är hämtade från Statistiska CentralByrån för vald tidpunkt och avser antalet invånare som är 18 år eller äldre. Ingen modifiering för eventuella skillnader i ålders- eller könsfördelning mellan landsdelarna är gjord."

#' @export
indikatorer_3 <-
    "Det förvalda diagrammet visar medianvärdet för patientens skattning av allmän hälsa på visuell-analogskala 0-100 mm (VAS global) för patienter med reumatoid artrit, 18 år eller äldre, vid start och vid ett uppföljningsbesök 4-12 månader efter start av ett första biologiskt läkemedel (bDMARD). Det förvalda diagrammet visar patienter som startat behandling under föregående år, visat per län och för riket. Diagrammet motsvarar en av indikatorerna i Socialstyrelsens Öppna jämförelser. Något målvärde finns inte fastställt. Ett lågt värde på VAS-skalan motsvarar god allmän hälsa.
Som startvärde har använts ett värde på VAS global som registrerats inom tidsintervallet 30 dagar före och 7 dagar efter behandlingsstart. Vid flera värden används det som ligger närmast behandlingsstart. Om startdatum inte är registrerat har ordinationsdatum använts som tidpunkt för start. För patienter med flera besök och registrerade värden på VAS global under uppföljningsperioden har det lägsta värdet använts.
Tidpunkten för behandlingsstart utgör urvalskriterium för patientgruppen som sedan följs. Också individer som saknar information om VAS global vid behandlingsstart bidrar med värden vid uppföljningsbesöket om sådant finns. Uppföljningsbesök fram till dagens datum, men maximalt 6 månader efter behandlingsstart är inkluderade för patientgruppen. Patienter som saknar värde på VAS global vid uppföljningen kan bidra med värden från behandlingsstart. Antal individer för vilka data saknas vid vardera besökstillfället finns presenterat i tabellen under fliken 'Tabell'.
Annat tidsintervall för startår, data för endast män respektive kvinnor och åldersintervall vid behandlingsstart är valbart. Det går att välja att visa staplar för endast medianvärden för VAS global vid behandlingsstart eller vid uppföljning. Länen kan sorteras efter länsnummer eller medianvärde. De data som ligger till grund för diagrammet ses under fliken 'Tabell'.
De läkemedel som inkluderas i begreppet biologiska läkemedel (bDMARD) kan ses under XXX."

#' @export
indikatorer_4 <-
    "Det förvalda diagrammet visar andel patienter med reumatoid artrit som uppnått minst låg sjukdomsaktivitet mätt med DAS28 av alla som har ett registrerat värde 4-12 månader efter start av ett första bDMARD. Måttet utgör en av Socialstyrelsens indikatorer. Diagrammet visar patienter fördelade på län och för hela riket.
Tidsperiod för starttidpunkten kan väljas. För patienter med flera besök och registrerade värden för utfallsmåttet under uppföljningsperioden har det lägsta värdet använts. Det går också att välja att använda sjukdomsaktivitetsmåttet CDAI, data för endast män respektive kvinnor och åldersintervall vid besöket. Länen kan sorteras efter länsnummer eller medianvärde.
De data som ligger till grund för diagrammet ses under fliken 'Tabell'."

#' @export
behandling_1 <-
    "Det förvalda diagrammet visar antalet inkluderade individer, 18 år eller äldre, med alla diagnoser som igår hade en registrerad, pågående behandling med ett biologiskt läkemedel (bDMARD) visat per län. Inom stapeln visas diagnoser för de ingående patienterna.
Annan tidpunkt, data för endast män respektive kvinnor, personernas ålder, diagnos samt typ av DMARD är valbart. Länen kan sorteras efter länsnummer eller antal individer.
De data som ligger till grund för diagrammet ses under fliken 'Tabell'. De läkemedel som inkluderas i begreppen biologiska läkemedel (bDMARD) och sDMARD kan ses under XXX.
I undergrupper med färre än 5 individer visas inga data i diagram eller tabell. Dessa grupper rapporteras i tabellet med benämningen YYY."

#' @export
behandling_2 <-
    "Det förvalda diagrammet visar antalet inkluderade individer, 18 år eller äldre, med reumatoid artrit som vid tidpunkten hade en registrerad, pågående behandling med ett biologiskt läkemedel (bDMARD) per 100000 invånare, visat per län och för riket. Diagrammet motsvarar en av indikatorerna i Socialstyrelsens Öppna jämförelser. Det vertikala intervallet representerar målvärdet och motsvarar genomsnittet för riket ±25%.
Annan tidpunkt, jämförelsetidpunkt, diagnos, data för endast män respektive kvinnor, ålder hos patienterna samt om antal individer eller antal individer/100000 invånare ska visas är valbart. Länen kan sorteras efter länsnummer eller antal individer.
De data som ligger till grund för diagrammet ses under fliken 'Tabell'. De läkemedel som inkluderas i begreppen biologiska läkemedel (bDMARD) kan ses under XXX.
I undergrupper med färre än 10 individer visas inga data i diagram eller tabell.
Uppgifterna om folkmängden är hämtade från Statistiska CentralByrån för vald tidpunkt och avser antalet invånare som är 18 år eller äldre. Ingen modifiering för eventuella skillnader i ålders- eller könsfördelning mellan landsdelarna är gjord."

#' @export
behandling_3 <-
    "Det förvalda diagrammet visar procentuell andel patienter med reumatoid artrit och nu pågående bDMARD som hade minst låg sjukdomsaktivitet mätt med DAS28 vid den senaste registreringen av alla under det senaste året. Diagrammet visar patienter fördelade på län och för hela riket.
Procenttalet grundas på det senaste besöket under året med ett registrerat DAS28-värde för varje individ. Besök vid start av bDMARD (0-30 dagar från starten) undantas för att utesluta startvärden och undvika värden från besök då bDMARD inte hunnit startas. Personer som avslutat behandlingen ingår inte i analysen. Annan tidpunkt kan väljas och utgör då den tidpunkt då bDMARD ska vara pågående. Det går också att välja en längre uppföljningsperiod, att använda sjukdomsaktivitetsmåttet CDAI, data för endast män respektive kvinnor och åldersintervall vid besöket. Länen kan sorteras efter länsnummer eller medianvärde.
Antalet valida registreringar anges i diagrammet inom parentes.
De data som ligger till grund för diagrammet ses under fliken 'Tabell'."

#' @export
behandling_4 <-
    "Diagrammet visar medianvärdet för patientens skattning av allmän hälsa på visuell-analogskala 0-100 mm (VAS global) vid start och vid ett uppföljningsbesök efter start av ett första biologiskt DMARD (bDMARD) och/eller första konventionellt syntetiskt DMARD (csDMARD). Diagrammet visar patienter fördelade på län och för hela riket.
Som startvärde har använts ett värde på VAS global som registrerats inom tidsintervallet 30 dagar före och 7 dagar efter behandlingsstart. Vid flera värden används det som ligger närmast behandlingsstart. Om startdatum inte är registrerat har ordinationsdatum använts som tidpunkt för start. För patienter med flera besök och registrerade värden på VAS global under uppföljningsperioden har det lägsta värdet använts.
Tidpunkten för behandlingsstart utgör urvalskriterium för patientgruppen som sedan följs. Också individer som saknar information om VAS global vid behandlingsstart bidrar med värden vid uppföljningsbesöket om sådant finns. Uppföljningsbesök fram till dagens datum, men maximalt 6 månader efter behandlingsstart är inkluderade för patientgruppen. Patienter som saknar värde på VAS global vid uppföljningen kan bidra med värden från behandlingsstart. Antal individer för vilka data saknas vid vardera besökstillfället finns presenterat i tabellen under fliken 'Tabell'.
'bDMARD', 'csDMARD' eller 'bDMARD eller csDMARD' kan väljas. Observera att valet 'bDMARD eller csDMARD' innebär att för en patient som startat flera typer av DMARD under perioden analyseras endast det första preparatet. Annat tidsintervall för startår, tidsintervall för uppföljning, diagnos, data för endast män respektive kvinnor och åldersintervall vid behandlingsstart är valbart. Det går att välja att visa staplar för endast medianvärden för VAS global vid behandlingsstart eller vid uppföljning. Länen kan sorteras efter länsnummer eller medianvärde. De data som ligger till grund för diagrammet ses under fliken 'Tabell'.
De läkemedel som inkluderas i begreppen biologiska läkemedel (bDMARD) och konventionella syntetiska DMARD (csDMARD) kan ses under XXX."

#' @export
inklusionsmatt_1 <-
    "Det förvalda diagrammet visar andel inkluderade individer, 18 år eller äldre vid inklusion, med tidig reumatoid artrit (<13 månader från symtomdebut/sjukdomsdebut till inklusion) som vid tidpunkt för inklusion/diagnos hade en sjukdomsduration på 20 veckor eller mindre, visat per län och för riket, för individer inkluderade under hela det föregående året. Totalt antal individer i analysen visas inom parentes och felstaplarna representerar 95 % konfidensintervall.
Diagrammet motsvarar en av indikatorerna i Socialstyrelsens Öppna jämförelser. Den vertikala linjen vid 50 % representerar målvärdet, att andelen patienter med reumatoid artrit som nydiagnosticeras inom 20 veckor bör vara ≥50 %.
Tidpunkten för inklusion/diagnos definieras som det tidigaste av datumen för inklusion och diagnos, eller datum för inklusion om data saknas för variabeln 'Datum för diagnos'.
Annat tidsintervall, åldergrupp, datum för inklusion eller diagnos, tidsperiod för indikatorn, diagnos, data för endast män respektive kvinnor, samt om konfidensintervall eller procenttal ska visas är valbart. Länen kan sorteras efter länsnummer eller procenttal. Som definition av tidig spondartrit, tidig ankyloserande spondylit respektive tidig psoriasisartrit har valts <60 månader från symtomdebut/sjukdomsdebut till inklusion. De data som ligger till grund för diagrammet ses under fliken 'Tabell'."

#' @export
kvalitetssakring_1 <-
    "Det förvalda diagrammet visar andelen inkluderade individer, 18 år eller äldre, oavsett diagnos, som har ett registrerat uppföljningsbesök 2-6 månader efter att ha startat sitt första biologiska läkemedel under föregående år, visat per län och för riket. Patienter med < 6 månader sedan ordination ingår inte i analysen.
'Biologiskt läkemedel', 'DMARD' eller 'Biologiskt läkemedel eller DMARD' kan väljas. Observera att valet 'Biologiskt läkemedel eller DMARD' innebär att samma patient kan komma att analyseras två gånger, en gång vid start av första DMARD och en gång vid start av första biologiska behandling.
Annat tidsintervall för ordination, tidsintervall för uppföljning, diagnos, data för endast män respektive kvinnor och åldersintervall vid ordination är valbart. Länen kan sorteras efter länsnummer eller antal individer. De individer som har kortare tid från ordination till dagens datum än maximalt antal månader i uppföljningsintervallet utesluts från analysen.
De data som ligger till grund för diagrammet ses under fliken 'Tabell'."

#' @export
kvalitetssakring_2 <-
    "Diagrammet visar det procentuella antalet individer som var inkluderade i SRQ med en diagnos inom diagnosgruppen 'Reumatoid artrit' av de som hade diagnosticerad reumatoid artrit enligt den definition som beskrivs nedan, vid vald tidpunkt.
Diagrammet kan presenteras sorterat på procenttal eller på länskod och för vissa år finns data för jämförelse över tid. Täckningsgraden kan också presenteras i form av en karta.
Eftersom nämnaren (antal patienter med diagnosticerad reumatoid artrit och aktiv behandling) baseras på bearbetade data från Patientregistret och Läkemedelsregistret finns endast grund för beräkningarna för vissa specifika tidpunkter. Dessa data är framtagna i samarbete mellan SRQ och Socialstyrelsen.
Från 2016 och framåt har definitionen av nämnaren (vår avsedda registerpopulation) förändrats något, då patienter som endast har diagnoskoderna M062 och M063 inte tas med. När data uppdateras för det senaste året sker också en uppdatering med aktuella siffror för de två föregående åren.
De data som ligger till grund för diagrammet ses under fliken 'Tabell'.
Definition av en individ med diagnosticerad reumatoid artrit.
Inklusionskriterier:
- Minst två vårdtillfällen eller besök med diagnos: seropositiv reumatoid artrit, seronegativ reumatoid artrit, annan specificerad reumatoid artrit, reumatoid artrit, ospecificerad eller palindrom reumatism (ICD 10-koder: M05, M060, M068, M069, M123), varav minst ett på en reumatologisk eller internmedicinsk specialistmottagning (MVO 101, 131 eller 311 i Jönköping. MVO 101, 131 eller 551 i Jämtland. MVO 101 eller 131 i övriga landet).
- Under det aktuella året minst en gång ha hämtat ut ett förskrivet biologiskt eller syntetiskt DMARD (ATC-koder: L01XC02, L04AA24, L04AB01, L04AB02, L04AB04, L04AB05, L04AB06, L04AC03, L04AC07, L04AC14, A07EC01, L04AA13, L04AD01, L04AX01, L04AX03, M01CB01, M01CB03, P01BA01, P01BA02, L04AA29, L04AA37).
Exklusionskriterium:
- Någonsin haft registrerad diagnos: juvenil artrit, ankyloserande spondylit, psoriasisartrit, systemisk lupus erytematosus eller inflammatorisk spondylopati •(M08, M09, M45, L405, M070, M071, M073, M320, M321, M328, M329, M460, M468, M469)"

#' @export
icd_ra <- list(
    header = "Reumatoid artrit (RA)",
    codes = c(
        "M05.3 Reumatoid artrit med engagemang av andra organ",
        "M05.8L Erosiv reumatoid artrit, RF-positiv och ACPA-positiv",
        "M05.8M Erosiv reumatoid artrit, RF-positiv och ACPA-negativ",
        "M05.8N Erosiv reumatoid artrit, RF-positiv och ACPA ospecificerad",
        "M05.9 Reumatoid artrit, seropositiv",
        "M05.9L Reumatoid artrit, RF-positiv och ACPA-positiv",
        "M05.9M Reumatoid artrit, RF-positiv och ACPA-negativ",
        "M05.9N Reumatoid artrit, RF-positiv och ACPA ospecificerad",
        "M06.0 Reumatoid artrit, seronegativ",
        "M06.0L Reumatoid artrit, RF-negativ och ACPA-positiv",
        "M06.0M Reumatoid artrit, RF-negativ och ACPA-negativ",
        "M06.0N Reumatoid artrit, RF-negativ och ACPA ospecificerad",
        "M06.8L Erosiv reumatoid artrit, RF-negativ och ACPA-positiv",
        "M06.8M Erosiv reumatoid artrit, RF-negativ och ACPA-negativ",
        "M06.8N Erosiv reumatoid artrit, RF-negativ och ACPA ospecificerad",
        "M06.9 Reumatoid artrit UNS",
        "M12.3 Palindrom reumatism"
    )
)

#' @export
icd_as <- list(
    header = "Ankyloserande spondylit (AS)",
    codes = "M45.9 Ankyloserande spondylit"
)

#' @export
icd_spa <- list(
    header = "Spondartrit (SpA)",
    codes = c(
        "M46.8 Spondylartrit",
        "M46.9 Inflammatorisk spondylopati, ospecificerad",
        "M08.1 Juvenil spondylit/spondylartrit"
    )
)

#' @export
icd_psa <- list(
    header = "Psoriasisartrit (PsA)",
    codes = c(
        "M07.0 Psoriatisk artrit i distal interfalangealled (L40.5†)",
        "M07.1 Arthritis mutilans (L40.5†)",
        "M07.2 Spondylit vid psoriasis (L40.5†)",
        "M07.3X Psoriasisartrit med annan eller ospecificerad lokalisation (L40.5†)",
        "M09.0 Juvenil artrit vid psoriasis (L40.5†)"
    )
)
