box::use(
    sh = shiny,
    bsl = bslib,
)

box::use(
    aui = app / logic / aux_ui,
    ase = app / logic / aux_server,
)

#' @export
ui <- function(id) {
    ns <- sh$NS(id)
    sh$tagList(
        sh$div(
            class = "d-flex flex-column align-items-center",
            sh$div(aui$return_button(ns("return"))),
            sh$div("this is VAP indikatorer 1")
        )
    )
}

#' @export
server <- function(id) {
    sh$moduleServer(id, function(input, output, session) {
        ase$observe_return(input)
    })
}