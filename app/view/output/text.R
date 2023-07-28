box::use(
    sh = shiny,
)

box::use(
    aui = app / logic / aux_ui,
)

#' @export
ui <- function(id) {
    ns <- sh$NS(id)
    aui$card(
        header = sh$div(class = "py-card-header", "Sammanfattning"),
        body = sh$textOutput(ns("text")),
        footer = sh$tags$button(class = "btn btn-secondary hover", sh$icon("ghost"), "Boo!")
    )
}

#' @export
server <- function(id, summary) {
    sh$moduleServer(id, function(input, output, session) {
        # Create summary text output
        output$text <- sh$renderText(summary)
    })
}
