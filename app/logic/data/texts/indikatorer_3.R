box::use(
    sh = shiny,
)

#' @export
indikatorer_3 <- sh$div(
    sh$h4("Det här visar det förvalda diagrammet"),
    sh$tags$hr(),
    sh$p("Det förvalda diagrammet visar medianvärdet för patientens skattning av allmän hälsa på visuell-analogskala 0-100mm (VAS global) för patienter"),
    sh$tags$ul(
        sh$tags$li("som har reumatoid artrit"),
        sh$tags$li("är 18 år eller äldre"),
        sh$tags$li("med start av ett första biologiska läkemedel (bDMARD) inklusive ett uppföljningsbesök 4-12 månader senare.")
    ),
    sh$p("Data visas per län och för riket, för patienter som startat behandling under föregående år. Något målvärde finns inte fastställt. Ett lågt värde på VAS-skalan motsvarar god allmän hälsa."),
    sh$p("Diagrammet motsvarar en av indikatorerna i Socialstyrelsens ", sh$tags$b("Öppna jämförelser"), "."),
    sh$p("Som startvärde har använts ett värde på VAS global som registrerats inom tidsintervallet 30 dagar före och 7 dagar efter behandlingsstart. Vid flera värden används det som ligger närmast behandlingsstart. Om startdatum inte har registrerat har ordiationsdatum använts som tidpunkt för start. För patienter med flera besök och registrerade värden på VAS global under uppföljningsperioden har den lägsta värdet använts."),
    sh$p("Tidpunkten för behandlingsstart ugör urvalskriterium för patientgruppen som sedan följs. Också individer som saknar information om VAS global vid behandlingsstart bidrar med värden vid uppföljningsbesöket om sådant finns. Uppföljningsbesök fram till dagens datum, men maximalt 6 månader efter behandlingsstart är inkluderade för patientgruppen. Patienter som saknar värde på VAS global vid uppföljningen kan bidra med värden från behandlingsstart. Antal individer för vilka data saknas vid vardera besökstillfället finns presenterat i tabellen under fliken ", sh$tags$b("Tabell"), "."),
    sh$h4("Visa annan data"),
    sh$tags$hr(),
    sh$tags$p("Du kan välja vilken data som visas genom att andända följande variabler:"),
    sh$tags$ul(
        sh$tags$li("startår"),
        
        sh$tags$li("diagnos"),
        sh$tags$li("data för endast män eller kvinnor"),
        sh$tags$li("åldersintervall vid behandlingsstart.")
    ),
    sh$p("Det går att väja att visa staplar för endast medianvärden för VAS gloal vid behandlingsstart eller uppföljning. Du kan sortera länen efter länsnummer eller värde."),
    sh$p("Data som ligger till grund för diagrammet kan du se under fliken ", sh$tags$b("Tabell"), ". De läkemedel som inkluderas i begreppen biologiska läkemedel (bDMARD) kan du se under ", sh$tags$b("Om preparatgrupper"), " på startsidan."),
    sh$p("Uppgifterna om folkmängden är hämtade från Statistika Centralbyrån för vald tidpunkt och avser antalet invånare som är 18 eller äldre. Ingen modifiering för eventuella skillnader i ålders- eller könsfördelning melan landsdelarna är gjord.")
)
