box::use(
    sh = shiny,
)

#' @export
inklusionsmatt_1 <- sh$div(
    sh$h4("Det här visar det förvalda diagrammet"),
    sh$tags$hr(),
    sh$p("Det förvalda diagrammet visar andel inkluderade individer som:"),
    sh$tags$ul(
        sh$tags$li("är 18 eller äldre vid inklusion"),
        sh$tags$li("har tidig reumatoid artrit (< 13 månader från symtomdebut/sjukdomsdebut till inklusion)"),
        sh$tags$li("vid tidpunkt för inklusion/diagnos hade en sjukdomsduration på 20 veckor eller mindre.")
    ),
    sh$p("Data visas per län och för riket, för individer inkluderade under hela det föregående året."),
    sh$p("Diagrammet motsvarar en av indikatorerna i Socialstyrelsens", sh$tags$b("Öppna jämförelser"), "."),
    sh$p("Den vertikala linjen vid 50 % representerar målvärdet, att andelen patienter med reumatoid artrit som nydiagnosticeras inom 20 veckor bör vara ≥50 %."),
    sh$p("Tidpunkten för inklusion/diagnos definieras som det tidigaste av datumen för inklusion och diagnos, eller datum för inklusion om data saknas för variabeln 'Datum för diagnos'."),
    sh$h4("Visa annan data"),
    sh$tags$hr(),
    sh$p("Du kan välja vilken data som visas genom att använda följande variabler:"),
    sh$tags$ul(
        sh$tags$li("tidsintervall"),
        sh$tags$li("åldersgrupp"),
        sh$tags$li("datum för inklusion eller diagnos"),
        sh$tags$li("tidsperiod för indikatorn"),
        sh$tags$li("diagnos"),
        sh$tags$li("data för endast män eller kvinnor."),
    ),
    sh$tags$p("Länen kan sorteras efter länsnummer eller procenttal. Som definition av tidig spondartrit, tidig ankyloserande spondylit respektive tidig psoriasisartrit har valts <60 månader från symtomdebut/sjukdomsdebut till inklusion. "),
    sh$p("Data som ligger till grund för diagrammet ses under fliken ", sh$tags$b("Tabell"), "."),
)
