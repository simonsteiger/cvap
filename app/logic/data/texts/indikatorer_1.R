box::use(
    sh = shiny,
)

#' @export
indikatorer_1 <- sh$div(
    sh$h4("Det här visar det förvalda diagrammet"),
    sh$tags$hr(),
    sh$p("Det förvalda diagrammet visar andel inkluderade individer som:"),
    sh$tags$ul(
        sh$tags$li("är 18 år eller äldre vid inklusion"),
        sh$tags$li("har tidig reumatoid artrit (det vill säga mindre än 13 månader från symtom- eller sjukdomsdebut till inklusion)"),
        sh$tags$li("hade en sjukdomsduration på 20 veckor eller mindre vid tidpunkten för inklusion/diagnos.")
    ),
    sh$p("Data visas per län och för riket, för individer inkluderade under hela det föregående året. Det vertikala linjen vid 50% representerar målvärdet – att andelen patienter reumatoid artrit som nydiagnosticeras inom 20 veckor bör vara 50 procent eller mer."),
    sh$p("För att bestämma ", sh$tags$b("tidpunkten för inklusion/diagnos"), " används det tidigaste av datumen för inklusion och diagnos. Saknas data för variabeln ", sh$tags$b("datum för diagnos"), " används data för ", sh$tags$b("datum för inklusion"), "."),
    sh$p("Diagrammet motsvarar en av indikatorerna i Socialstyrelsens ", sh$tags$b("Öppna jämförelser"), "."),
    sh$h4("Visa annan data"),
    sh$tags$hr(),
    sh$tags$p("Du kan välja vilken data som visas genom att andända följande variabler:"),
    sh$tags$ul(
        sh$tags$li("tidsintervall"),
        sh$tags$li("åldersintervall vid inklusion"),
        sh$tags$li("datum för inklusion eller diagnos"),
        sh$tags$li("data för endast män eller kvinnor.")
    ),
    sh$p("Du kan sortera länen efter länsnummer eller värde."),
    sh$p("Data som ligger till grund för diagrammet kan du se under fliken ", sh$tags$b("Tabell"), ".")
)
