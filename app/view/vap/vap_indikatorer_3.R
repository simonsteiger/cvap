box::use(
    stats,
    magrittr[`%>%`],
    sh = shiny,
    bsl = bslib,
    bsi = bsicons,
    dp = dplyr,
    rtbl = reactable,
    e4r = echarts4r,
    em = echarts4r.maps,
    sw = shinyWidgets,
    ht = htmltools,
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
        aui$inp_daterange(sh$NS(ns("sift"), "ordinerat"), "Välj datum"),
        aui$inp_radio_sex(sh$NS(ns("sift"), "kon")),
        aui$inp_slider_age(sh$NS(ns("sift"), "ar"))
    )

    sh$tagList(
        aui$container_fluid(
            aui$row(
                left = sh$div(aui$btn_return(ns("return"))),
                center = aui$head()
            ),
            aui$row(
                center = sh$div(
                    aui$layout_column_wrap(
                        grid_template_columns = "1fr 4fr",
                        aui$card(
                            header = "Inputs",
                            body = sift$ui(ns("sift"), !!!inputs)
                        ),
                        aui$card(
                            header = "Barplot",
                            body = e4r$echarts4rOutput(ns("bar"))
                        ),
                    ),
                    aui$layout_column_wrap(
                        grid_template_columns = "2fr 1fr",
                        aui$card(
                            header = "Table",
                            body = rtbl$reactableOutput(ns("table"))
                        ),
                        aui$card(
                            header = "Map",
                            body = e4r$echarts4rOutput(ns("map"))
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
            .by = c("lan", "ordinerat"),
            na.rm = TRUE
        )

        output$table <- rtbl$renderReactable({
            sh$req(is.data.frame(synopsis()))
            rtbl$reactable(synopsis())
        })

        output$bar <- e4r$renderEcharts4r({
            sh$req(is.data.frame(synopsis()))
            synopsis() %>%
                ase$plot_bar(
                    x = "lan",
                    y = "patientens_globala",
                    group = "ordinerat"
                )
        })

        output$map <- e4r$renderEcharts4r({
            sh$req(is.data.frame(synopsis()))
            synopsis() %>%
                ase$plot_map(
                    geo = geo$sweden_json_small,
                    x = "lan",
                    y = "patientens_globala",
                    group = "ordinerat"
                )
        })
    })
}
