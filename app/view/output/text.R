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
        footer = sh$htmlOutput(ns("modal"))
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
                label = sh$div(class = "d-flex flex-row gap-1 align-items-center", sh$icon("book-medical"), "ICD-10 kods"),
                modal_title = "Information om diagnoser",
                footer_confirm = NULL,
                footer_dismiss = "Tillbaka",
                ase$icd_compose(summary$extra_info$icds)
            )
        )
    })
}
