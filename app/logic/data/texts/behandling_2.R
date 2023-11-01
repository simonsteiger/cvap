box::use(
    sh = shiny,
)

#' @export
behandling_2 <- sh$div(
    sh$h4("Det här visar det förvalda diagrammet"),
    sh$tags$hr(),
    sh$p("Det förvalda diagrammet visar antalet inkluderade individer:"),
    sh$tags$ul(
        sh$tags$li("är 18 år eller äldre"),
        sh$tags$li("med reumatoid artrit som vid tidpunkten hade en registrerad, pågående behandling med ett biologiskt läkemedel (bDMARD).")
    ),
    sh$p("Data visas per 100 000 invånare och per län och för riket. Diagrammet motsvara en av indikatorerna i Socialstyrelsens ", sh$tags$b("Öppna jämförelser"), "."),
    sh$p("Det vertikala intervallet representerar målvärdet och motsvarar genomsnittet för riket ± 25%."),
    sh$h4("Visa annan data"),
    sh$tags$hr(),
    sh$tags$p("Du kan välja vilken data som visas genom att andända följande variabler:"),
    sh$tags$ul(
        sh$tags$li("tidpunkt"),
        sh$tags$li("jämförelsetidpunkt"),
        sh$tags$li("diagnos"),
        sh$tags$li("data för endast män eller kvinnor"),
        sh$tags$li("äldersintervall"),
        sh$tags$li("om antal individer eller antal individer per 100 000 invånare ska visas eller inte.")
    ),
    sh$p("Länen kan sorteras efter länsnummer eller antal individer."),
    sh$p("Data som ligger till grund för diagrammet ses under fliken ", sh$tags$b("Tabell"), ". De läkemedel som inkluderas i begreppen biologiska läkemedel (bDMARD) kan ses under ", sh$tags$b("Om preparatgrupper"), "på startsidan."),
    sh$p("Uppgifterna om folkmängden är hämtade från Statistiska CentralByrån för vald tidpunkt och avser antalet invånare som är 18 år eller äldre. Ingen modifiering för eventuella skillnader i ålders- eller könsfördelning mellan landsdelarna är gjord.")
)
