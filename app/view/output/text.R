box::use(
    sh = shiny,
)

box::use(
    aui = app / logic / aux_ui,
    ase = app / logic / aux_server,
)

#' @export
ui <- function(id) {
    ns <- sh$NS(id)
    aui$card(
        header = sh$div(class = "py-card-header", "Sammanfattning"),
        body = sh$htmlOutput(ns("text")),
    )
}

#' @export
server <- function(id, summary = list()) {
    sh$moduleServer(id, function(input, output, session) {
        # Create summary text output
        output$text <- sh$renderUI(summary$text)
    })
}
