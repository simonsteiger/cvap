box::use(
    sh = shiny,
)

#' @export
indikatorer_2 <- sh$div(
    sh$h4("Det här visar det förvalda diagrammet"),
    sh$tags$hr(),
    sh$p("Det förvalda diagrammet visar andel inkluderade individer som:"),
    sh$tags$ul(
        sh$tags$li("är 18 år eller äldre vid inklusion"),
        sh$tags$li("har reumatoid artrit och vid tidpunkten hade en registrerad, pågående behandling med ett biologiskt läkemedel (bDMARD).")
    ),
    sh$p("Data visas per 100 000 invånare, län och för riket. Det vertikala intervallet representerar målvärdet och motsvarar genomsnittet för riket ± 25%."),
    sh$p("Diagrammet motsvarar en av indikatorerna i Socialstyrelsens ", sh$tags$b("Öppna jämförelser"), "."),
    sh$h4("Visa annan data"),
    sh$tags$hr(),
    sh$tags$p("Du kan välja vilken data som visas genom att andända följande variabler:"),
    sh$tags$ul(
        sh$tags$li("tidsintervall"),
        sh$tags$li("jämförelsetidpunkt"),
        sh$tags$li("diagnos"),
        sh$tags$li("åldersintervall vid inklusion"),
        sh$tags$li("data för endast män eller kvinnor.")
    ),
    sh$p("Du kan sortera länen efter länsnummer eller värde."),
    sh$p("Data som ligger till grund för diagrammet kan du se under fliken ", sh$tags$b("Tabell"), ". De läkemedel som inkluderas i begreppen biologiska läkemedel (bDMARD) kan du se under ", sh$tags$b("Om preparatgrupper"), " på startsidan."),
    sh$p("Uppgifterna om folkmängden är hämtade från Statistika Centralbyrån för vald tidpunkt och avser antalet invånare som är 18 eller äldre. Ingen modifiering för eventuella skillnader i ålders- eller könsfördelning melan landsdelarna är gjord.")
)
