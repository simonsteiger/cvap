box::use(
    sh = shiny,
    dp = dplyr,
    bsl = bslib,
)

#' @export
ui <- function(id) {
    ns <- sh$NS(id)
    sh$tagList()
}

#' @export
server <- function(id) {
    sh$moduleServer(id, function(input, output, session) {
        icon_kon <- sh$reactive(
            dp$case_match(input$kon,
                "Kvinna" ~ sh$tagList(sh$icon("venus")),
                "Man" ~ sh$tagList(sh$icon("mars")),
                .default = sh$tagList(sh$icon("venus-mars"))
            )
        )

        icon_alder <- sh$reactive(
            dp$case_when(
                max(input$alder) < 40 ~ sh$tagList(sh$icon("children")),
                min(input$alder) > 70 ~ sh$tagList(sh$icon("person-cane")),
                .default = sh$tagList(sh$icon("people-group"))
            )
        )

        sh$reactive(
            sh$tagList(
                icon_alder(), paste0(paste0(input$alder, collapse = " till "), " år"),
                sh$hr(),
                icon_kon(), input$kon,
                sh$hr(),
                sh$icon("calendar"), paste0(input$inkluderad, collapse = " till ")
            )
        )
    })
}

# div(
#   class = "card-sammanfattning",
#   card(
#     card_header(
#       class = "bg-dark",
#       "Översikt"
#     ),
#     card_body(
#       height = 200,
#       icon_age(), paste0(paste0(input$age, collapse = " till "), " år"),
#       hr(),
#       icon_sex(), input$sex,
#       hr(),
#       icon("calendar"), paste0(input$timeframe, collapse = " till ")
#     )
#   )
# )
