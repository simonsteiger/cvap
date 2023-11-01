box::use(
    sh = shiny,
)

#' @export
kvalitetssakring_2 <- sh$div(
    sh$h4("Det här visar det förvalda diagrammet"),
    sh$tags$hr(),
    sh$p("Diagrammet visar det procentuella antalet individer som var inkluderade i SRQ med en diagnos inom diagnosgruppen ", sh$tags$b("Reumatoid artrit"), "av de som hade diagnosticerad reumatoid artrit enligt den definition som beskrivs nedan, vid vald tidpunkt."),
    sh$p("Diagrammet kan presenteras sorterat på procenttal eller på länskod och för vissa år finns data för jämförelse över tid. Täckningsgraden kan också presenteras i form av en karta."),
    sh$p("Eftersom nämnaren (antal patienter med diagnosticerad reumatoid artrit och aktiv behandling) baseras på bearbetade data från Patientregistret och Läkemedelsregistret finns endast grund för beräkningarna för vissa specifika tidpunkter. Dessa data är framtagna i samarbete mellan SRQ och Socialstyrelsen."),
    sh$p("Från 2016 och framåt har definitionen av nämnaren (vår avsedda registerpopulation) förändrats något, då patienter som enbart har diagnoskoderna M062 och M063 inte tas med. När data uppdateras för det senaste året sker också en uppdatering med aktuella siffror för de två föregående åren."),
    sh$p("Data som ligger till grund för diagrammet ses under fliken ", sh$tags$b("Tabell"), "."),
    sh$h4("Definition av en individ med diagnosticerad reumatoid artrit"),
    sh$hr(),
    sh$h5("Inklusionskriterier"),
    sh$tags$ol(
        sh$tags$li(
            "Minst två vårdtillfällen eller besök, varav minst ett på en reumatologisk eller internmedicinsk specialistmottagning (MVO 101, 131 eller 311 i Jönköping. MVO 101, 131 eller 551 i Jämtland. MVO 101 eller 131 i övriga landet) med någon av följande diagnoser:",
            sh$tags$ul(
                sh$tags$li("seropositiv reumatoid artrit (ICD 10-kod: M05)"),
                sh$tags$li("seronegativ reumatoid artrit (ICD 10-kod: M060)"),
                sh$tags$li("annan specificerad reumatoid artrit (ICD 10-kod: M068)"),
                sh$tags$li("reumatoid artrit (ICD 10-kod: M069), ospecificerad eller palindrom reumatism (ICD 10-kod: M123)")
            ),
        ),
        sh$tags$li(
            sh$div(
                sh$p("Under det aktuella året minst en gang ha hämtat ut ett förskrivet biologiskt eller syntetiskt DMARD"),
                sh$p("ATC koder:")
            ),
            sh$tags$ul(
                sh$tags$li("L01XC02"),
                sh$tags$li("L04AA24"),
                sh$tags$li("L04AB01"),
                sh$tags$li("L04AB02"),
                sh$tags$li("L04AB04"),
                sh$tags$li("L04AB05"),
                sh$tags$li("L04AB06"),
                sh$tags$li("L04AC03"),
                sh$tags$li("L04AC07"),
                sh$tags$li("A07EC01"),
                sh$tags$li("L04AA13"),
                sh$tags$li("L04AD01"),
                sh$tags$li("L04AX01"),
                sh$tags$li("L04AX03"),
                sh$tags$li("P01BA01"),
                sh$tags$li("P01BA02"),
                sh$tags$li("L04AA29"),
                sh$tags$li("L04AA37"),
            )
        )
    ),
    sh$h5("Exlusionskriterium"),
    sh$p("Någonsin haft någon av följande diagnoser registrerad:"),
    sh$tags$ul(
        sh$tags$li("juvenil artrit"),
        sh$tags$li("ankyloserande spondylit"),
        sh$tags$li("psoriasisartrit"),
        sh$tags$li("systemisk lupus erytematosus"),
        sh$tags$li("inflammatorisk spondylopati"),
    ),
    sh$tags$p("(ICD-10 koderna: M09, M09, M45, L405, M070, M071, M073, M320, M321, M328, M329, M460, M468 och M469)"),
)
