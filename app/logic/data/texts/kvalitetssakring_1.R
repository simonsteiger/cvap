box::use(
    sh = shiny,
)

#' @export
kvalitetssakring_1 <- sh$div(
    sh$h4("Det här visar det förvalda diagrammet"),
    sh$tags$hr(),
    sh$p("Det förvalda diagrammet visar andelen inkluderade individer, 18 år eller äldre, oavsett diagnos, som har ett registrerat uppföljningsbesök 2–6 månader efter att ha startat sitt första biologiska läkemedel under föregående år."),
    sh$p("Data visas per län och för riket. Patienter med mindre än 6 månader sedan ordination ingår inte i analysen."),
    sh$h4("Visa annan data"),
    sh$tags$hr(),
    sh$p("Du kan välja vilken data som visas genom att använda följande variabler:"),
    sh$tags$ul(
        sh$tags$li("tidsintervall för ordination"),
        sh$tags$li("tidsintervall för uppföljning"),
        sh$tags$li("diagnos"),
        sh$tags$li("data för endast män eller kvinnor"),
        sh$tags$li("åldersintervall vid ordination.")
    ),
    sh$tags$p("Länen kan sorteras efter länsnummer eller antal individer."),
    sh$p("Det går också att välja:"),
    sh$tags$ul(
        sh$tags$li("Biologiskt läkemedel"),
        sh$tags$li("DMARD"),
        sh$tags$li("Biologiskt läkemedel eller DMARD.")
    ),
    sh$p("Observera att valet ", sh$p("Biologiskt läkemedel eller DMARD"), "innebär att samma patient kan komma att analyseras två gånger, en gång vid start av första DMARD och en gång vid start av första biologiska behandling."),
    sh$p("De individer som har kortare tid från ordination till dagens datum än maximalt antal månader i uppföljningsintervallet utesluts från analysen."),
    sh$p("Data som ligger till grund för diagrammet ses under fliken ", sh$tags$b("Tabell"), "."),
)
