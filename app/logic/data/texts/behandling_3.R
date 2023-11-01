box::use(
    sh = shiny,
)

#' @export
behandling_3 <- sh$div(
    sh$h4("Det här visar det förvalda diagrammet"),
    sh$tags$hr(),
    sh$p("Det förvalda diagrammet visar antalet inkluderade individer:"),
    sh$tags$ul(
        sh$tags$li("är 18 år eller äldre"),
        sh$tags$li("med reumatoid artrit"),
        sh$tags$li("med nu pågående bDMARD"),
        sh$tags$li("som hade minst låg sjukdomsaktivitet mätt med DAS28 vid den senaste registreringen av alla under det senaste året.")
    ),
    sh$p("Diagrammet visar patienter fördelade på län och för hela riket."),
    sh$p("Det vertikala intervallet representerar målvärdet och motsvarar genomsnittet för riket ± 25%."),
    sh$p("Procenttalet grundas på det senaste besöket under året med ett registrerat DAS28-värde för varje individ. Besök vid start av bDMARD (0-30 dagar från starten) undantas för att utesluta startvärden och undvika värden från besök då bDMARD inte hunnit startas. Personer som avslutat behandlingen ingår inte i analysen."),
    sh$h4("Visa annan data"),
    sh$tags$hr(),
    sh$tags$p("Annan tidpunkt kan väljas och utgör då den tidpunkt då bDMARD ska vara pågående. Det går också att välja en längre uppföljningsperiod, att använda sjukdomsaktivitetsmåttet CDAI, data för endast män respektive kvinnor och åldersintervall vid besöket. Länen kan sorteras efter länsnummer eller medianvärde."),
    sh$p("Länen kan sorteras efter länsnummer eller medianvärde."),
    #sh$p("Antalet valida registreringar anges i diagrammet inom parentes."),
    sh$p("Data som ligger till grund för diagrammet ses under fliken ", sh$tags$b("Tabell"), ". De läkemedel som inkluderas i begreppen biologiska läkemedel (bDMARD) kan ses under ", sh$tags$b("Om preparatgrupper"), "på startsidan."),
)
