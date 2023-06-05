box::use(
    sh = shiny,
    bsl = bslib,
)

box::use(
    aui = app / logic / aux_ui,
    ase = app / logic / aux_server,
    app / view / sift,
)

#' @export
ui <- function(id) {
    ns <- sh$NS(id)

    inputs <- sh$tagList(
        aui$inp_daterange(ns("daterange"), "Välj datum"),
        aui$inp_radio_sex(ns("sex")),
        aui$inp_slider_age(ns("age"))
    )

    sh$tagList(
        aui$container_fluid(
            aui$row(
                left = sh$div(aui$btn_return(ns("return"))),
                center = aui$head()
            ),
            aui$row(
                center = sh$div(
                    class = "d-flex flex-column align-items-center m-5",
                    aui$card(
                        "Behandling_3",
                        sidebar = bsl$sidebar(sift$ui("sift", !!!inputs)),
                        bsl$nav_panel(
                            "tab1",
                        ),
                        bsl$nav_panel(
                            "tab2",
                        ),
                        bsl$nav_panel(
                            "tab3",
                            "Just some text"
                        )
                    )
                )
            )
        )
    )
}

#' @export
server <- function(id, data) {
    sh$moduleServer(id, function(input, output, session) {
        ase$obs_return(input)

        sifted <- sh$reactive(sift$server("sift", data))
    })
}
