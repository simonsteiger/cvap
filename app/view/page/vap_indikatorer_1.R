box::use(
    sh = shiny,
    bsl = bslib,
)

box::use(
    aui = app / logic / aux_ui,
    ase = app / logic / aux_server,
    app / view / wrangle / sift,
)

#' @export
ui <- function(id) {
    ns <- sh$NS(id)
    sh$tagList(
        aui$container_fluid(
            aui$row(
                left = sh$div(aui$btn_return(ns("return"))),
                center = aui$head()
            ),
            aui$row(
                center = sh$div(
                    class = "d-flex flex-column align-items-center m-5",
                    aui$card("Indikatorer_1", bsl$nav_panel("bla", "test"))
                )
            )
        )
    )
}

#' @export
server <- function(id) {
    sh$moduleServer(id, function(input, output, session) {
        ase$obs_return(input)
    })
}