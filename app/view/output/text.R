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
        body = sh$textOutput(ns("text")),
        footer = sh$htmlOutput(ns("modal")) #sh$tags$button(class = "btn btn-secondary hover", sh$icon("ghost"), "Boo!")
    )
}

#' @export
server <- function(id, summary = list()) {
    sh$moduleServer(id, function(input, output, session) {
        # Create summary text output
        output$text <- sh$renderText(summary$text)
        output$modal <- sh$renderUI(
            aui$btn_modal(
                id = session$ns("summary_modal"),
                label = "Extra info",
                modal_title = "Extra info",
                footer_confirm = NULL,
                footer_dismiss = "Tillbaka",
                ase$icd_compose(summary$extra_info$icds)
            )
        )
    })
}
