box::use(
    sh = shiny,
)

#' @export
indikatorer_4 <- sh$div(
    sh$h4("Det här visar det förvalda diagrammet"),
    sh$tags$hr(),
    sh$p("Det förvalda diagrammet visar andel patienter:"),
    sh$tags$ul(
        sh$tags$li("med reumatoid artrit"),
        sh$tags$li("är 18 år eller äldre"),
        sh$tags$li("som uppnått minst låg sjukdomsaktivitet mätt med DAS28 av alla som har ett registrerat värde 4-12 månader efter start av ett första bDMARD."),
        sh$tags$li("med start av ett första biologiska läkemedel (bDMARD) inklusive ett uppföljningsbesök 4-12 månader senare.")
    ),
    sh$p("Måttet motsvarar en av Socialstyrelsens indikatorer. Diagrammet visar patienter fördelade på län och för hela riket."),
    sh$h4("Visa annan data"),
    sh$tags$hr(),
    sh$tags$p("Du kan välja vilken data som visas genom att andända följande variabler:"),
    sh$tags$ul(
        sh$tags$li("tidsfönster för ordinationsdatum"),
        sh$tags$li("utfallsmått"),
        sh$tags$li("data för endast män eller kvinnor"),
        sh$tags$li("åldersintervall vid behandlingsstart.")
    ),
    sh$p("För patienter med flera besök och registrerade värden för utfallsmåttet under uppföljningsperioden har det lägsta värdet använts. Det går också att välja att använda sjukdomsaktivitetsmåttet CDAI, data för endast män respektive kvinnor och åldersintervall vid besöket."),
    sh$p("Data som ligger till grund för diagrammet kan du se under fliken ", sh$tags$b("Tabell"), "."),
)
