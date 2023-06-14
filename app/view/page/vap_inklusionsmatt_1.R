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
            # aui$row_sidebar(
            #     sidebar = bsl$card(bsl$card_header("Side")),
            #     list(
            #         bsl$card(bsl$card_header("Main1")),
            #         bsl$card(bsl$card_header("Main2")),
            #         bsl$card(bsl$card_header("Main3")),
            #         bsl$card(bsl$card_header("Main4"))
            #     )
            # )
        )
    )
}

#' @export
server <- function(id) {
    sh$moduleServer(id, function(input, output, session) {
        ase$obs_return(input)
    })
}
