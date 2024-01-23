box::use(
    sh = shiny,
)

#' @export
info_bar <- sh$div(
    sh$tags$ul(
        sh$tags$li("Hovra över staplarna för att se exakta värden."),
        sh$tags$li("Klicka på teckenförklaringen för att dölja och visa respektive grupp."),
        sh$tags$li("Länen är sorterade efter länskod. Avmarkera ", sh$tags$b("Länskod"), " om du vill sortera efter värden."),
        sh$tags$li("Klicka på ", sh$tags$b("Ladda ner"), " för att ladda ner en version av den aktuella grafen i pdf-format.")
    )
)

#' @export
info_tbl <- sh$div(
    sh$tags$ul(
        sh$tags$li("Klicka på ", sh$tags$b("Ladda ner"), " för att ladda ner den aktuella tabellen i csv-format."),
        sh$tags$li("Om du använder en liten skärm, klicka på ", sh$tags$b("Expandera"), " knappen i det nedre högra hörnet för att kunna bläddra igenom alla sidor.")
    )
)

#' @export
info_map <- sh$div(
    sh$tags$ul(
        sh$tags$li("Hovra över ett län på kartan för att se länets namn och länets värde i teckenförklaringen."),
        sh$tags$li("Hovra över teckenförklaringen för att visa vilka län som har värden nära det du värde du hovrar över."),
        sh$tags$li("Klicka på ", sh$tags$b("Ladda ner"), " för att ladda ner en version av den aktuella grafen i pdf-format.")
    )
)