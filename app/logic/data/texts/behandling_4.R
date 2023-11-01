box::use(
    sh = shiny,
)

#' @export
behandling_4 <- sh$div(
    sh$h4("Det här visar det förvalda diagrammet"),
    sh$tags$hr(),
    sh$p("Diagrammet visar medianvärdet för patientens skattning av allmän hälsa på visuell-analogskala 0–100 mm (VAS global)"),
    sh$tags$ul(
        sh$tags$li("vid start"),
        sh$tags$li("vid ett uppföljningsbesök efter start av ett första biologiskt DMARD (bDMARD) och/eller första konventionellt syntetiskt DMARD (csDMARD)")
    ),
    sh$p("Diagrammet visar patienter fördelade på län och för hela riket."),
    sh$p("Som startvärde har använts ett värde på VAS global som registrerats inom tidsintervallet 30 dagar före och 7 dagar efter behandlingsstart. Vid flera värden används det som ligger närmast behandlingsstart. Om startdatum inte är registrerat har ordinationsdatum använts som tidpunkt för start. För patienter med flera besök och registrerade värden på VAS global under uppföljningsperioden har det lägsta värdet använts."),
    sh$p("Tidpunkten för behandlingsstart utgör urvalskriterium för patientgruppen som sedan följs. Också individer som saknar information om VAS global vid behandlingsstart bidrar med värden vid uppföljningsbesöket om sådant finns. Uppföljningsbesök fram till dagens datum, men maximalt 6 månader efter behandlingsstart, är inkluderade för patientgruppen. Patienter som saknar värde på VAS global vid uppföljningen kan bidra med värden från behandlingsstart. Antal individer för vilka data saknas vid vardera besökstillfälle finns presenterat i tabellen under fliken ", sh$tags$b("Tabell"), "."),
    sh$h4("Visa annan data"),
    sh$p("Du kan välja vilken data som visas genom att använda följande variabler:"),
    sh$tags$hr(),
    sh$tags$ul(
        sh$tags$li("annat tidsintervall för startår"),
        sh$tags$li("tidsintervall för uppföljning"),
        sh$tags$li("diagnos"),
        sh$tags$li("data för endast män respektive kvinnor"),
        sh$tags$li("åldersintervall vid behandlingsstart"),
    ),
    sh$tags$p("Det går att välja att visa staplar för endast medianvärden för VAS global vid behandlingsstart eller vid uppföljning. Länen kan sorteras efter länsnummer eller medianvärde."),
    sh$p("Det går också att välja:"),
    sh$tags$ul(
        sh$tags$li("bDMARD"),
        sh$tags$li("csDMARD"),
        sh$tags$li("bDMARD eller csDMARD"),
    ),
    sh$p("Observera att valet ", sh$tags$b("bDMARD eller csDMARD"), " innebär att endast det första preparatet analyseras för en patient som startat flera typer av DMARD under perioden."),
    sh$p("Data som ligger till grund för diagrammet ses under fliken ", sh$tags$b("Tabell"), ". De läkemedel som inkluderas i begreppen biologiska läkemedel (bDMARD) kan ses under ", sh$tags$b("Om preparatgrupper"), "på startsidan."),
)
