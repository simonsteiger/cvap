box::use(
    sh = shiny,
)

#' @export
behandling_1 <- sh$div(
    sh$h4("Det här visar det förvalda diagrammet"),
    sh$tags$hr(),
    sh$p("Det förvalda diagrammet visar antalet inkluderade individer:"),
    sh$tags$ul(
        sh$tags$li("är 18 år eller äldre"),
        sh$tags$li("med alla diagnoser som igår hade en registrerad, pågående behandling med ett biologiskt läkemedel (bDMARD).")
    ),
    sh$p("Data visas per län."),
    sh$h4("Visa annan data"),
    sh$tags$hr(),
    sh$tags$p("Du kan välja vilken data som visas genom att andända följande variabler:"),
    sh$tags$ul(
        sh$tags$li("tidpunkt"),
        sh$tags$li("äldersintervall"),
        sh$tags$li("data för endast män eller kvinnor"),
        sh$tags$li("diagnos"),
        sh$tags$li("typ av DMARD")
    ),
    sh$p("Länen kan sorteras efter länsnummer eller antal individer."),
    sh$p("Data som ligger till grund för diagrammet ses under fliken ", sh$tags$b("Tabell"), ". De läkemedel som inkluderas i begreppen biologiska läkemedel (bDMARD) och konventionella syntetiska läkemedel (csDMARD) kan ses under ", sh$tags$b("Om preparatgrupper"), "på startsidan."),
)
