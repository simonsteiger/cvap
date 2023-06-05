box::use(
    stats,
    magrittr[`%>%`],
    sh = shiny,
    bsl = bslib,
    bsi = bsicons,
    rtbl = reactable,
    e4r = echarts4r,
    em = echarts4r.maps,
)

box::use(
    aui = app / logic / aux_ui,
    ase = app / logic / aux_server,
    geo = app / logic / data / geojson,
    app / view / sift,
    app / view / synopsis,
)

#' @export
ui <- function(id) {
    ns <- sh$NS(id)

    inputs <- sh$tagList(
        aui$inp_daterange(sh$NS("sift", "ordinerat"), "Välj datum"),
        aui$inp_radio_sex(sh$NS("sift", "kon")),
        aui$inp_slider_age(sh$NS("sift", "ar"))
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
                        title = "Behandling_3",
                        sidebar = bsl$sidebar(sift$ui(ns("sift"), !!!inputs), width = 300),
                        bsl$nav_panel(
                            "tab1",
                            rtbl$reactableOutput(ns("table"))
                        ),
                        bsl$nav_panel(
                            "tab2",
                            e4r$echarts4rOutput(ns("bar"))
                        ),
                        bsl$nav_panel(
                            "tab3",
                            e4r$echarts4rOutput(ns("map"))
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

        sieve <- sift$server("sift", sh$reactive(data))

        synopsis <- synopsis$server(
            "summary",
            sh$reactive(data[sieve(), ]),
            .fn = stats$median,
            .var = "patientens_globala",
            .by = "lan",
            na.rm = TRUE
        )

        output$table <- rtbl$renderReactable({
            sh$req(is.data.frame(synopsis()))
            rtbl$reactable(synopsis())
        })

        output$bar <- e4r$renderEcharts4r({
            sh$req(is.data.frame(synopsis()))
            synopsis() %>%
                e4r$e_charts(lan) %>%
                e4r$e_bar(patientens_globala) %>%
                e4r$e_flip_coords()
        })

        output$map <- e4r$renderEcharts4r({
            sh$req(is.data.frame(synopsis()))
            synopsis() %>%
                e4r$e_charts(lan) %>%
                e4r$e_map_register("Sweden", geo$sweden_json_small) %>%
                e4r$e_map(patientens_globala, map = "Sweden", nameProperty = "NAME_1") %>%
                e4r$e_visual_map(patientens_globala) %>%
                e4r$e_theme("infographic")
        })
    })
}
