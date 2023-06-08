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
    app / view / wrangle / sift,
    app / view / wrangle / synopsis,
    app / view / output / table,
    app / view / output / bar,
    app / view / output / map,
)

#' @export
ui <- function(id) {
    ns <- sh$NS(id)

    inputs <- sh$tagList(
        aui$inp_daterange(sh$NS(ns("sift"), "ordinerat"), "Välj datum"),
        aui$inp_radio_sex(sh$NS(ns("sift"), "kon")),
        aui$inp_slider_age(sh$NS(ns("sift"), "alder"))
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
server <- function(id, data, geo) {
    sh$moduleServer(id, function(input, output, session) {
        ase$obs_return(input)

        sieve <- sift$server("sift", sh$reactive(data))

        synopsis <- synopsis$server(
            "summary",
            sh$reactive(data[sieve(), ]),
            .fn = stats$median,
            .var = "patientens_globala",
            .by = c("lan", "visit_group"),
            na.rm = TRUE
        )

        table <- table$server(
            "output",
            synopsis,
            arrange = c("lan", "visit_group")
        )

        bar <- bar$server(
            "output",
            synopsis,
            x = "lan",
            y = "patientens_globala",
            group = "visit_group"
        )

        map <- map$server(
            "output",
            synopsis,
            geo,
            x = "lan",
            y = "patientens_globala",
            group = "visit_group"
        )

        output$table <- rtbl$renderReactable(table())

        output$bar <- e4r$renderEcharts4r(bar())

        output$map <- e4r$renderEcharts4r(map())
    })
}
