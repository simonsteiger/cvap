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
        aui$inp_daterange(sh$NS(ns("sift"), "inkluderad"), "Välj tidsfönster för inklusionsdatum"),
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
                left = sh$div(aui$btn_dropdown(inputs)),
                center = sh$div(
                    aui$layout_column_wrap(
                        grid_template_columns = "3fr 2fr",
                        aui$card(
                            header = "Barplot",
                            # body = e4r$echarts4rOutput(ns("bar"))
                        ),
                        aui$card(
                            header = "Map",
                            # body = e4r$echarts4rOutput(ns("map"))
                        )
                    ),
                    aui$layout_column_wrap(
                        grid_template_columns = "2fr 3fr",
                        aui$card(
                            header = "Text",
                            # body = "Summary text"
                        ),
                        aui$card(
                            header = "Table",
                            # body = rtbl$reactableOutput(ns("table"))
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

        # sieve <- sift$server("sift", sh$reactive(data))
# 
        # dat_synopsis <- synopsis$server(
        #     "summary",
        #     sh$reactive(data[sieve(), ]),
        #     group = "inkluderad",
        #     .fn = mean,
        #     .var = "visit_group",
        #     .by = c("lan", "inkluderad"),
        #     na.rm = TRUE
        # )
# 
        # table <- table$server(
        #     "output",
        #     dat_synopsis,
        #     arrange = c("lan", "inkluderad")
        # )
# 
        # bar <- bar$server(
        #     "output",
        #     dat_synopsis,
        #     x = "lan",
        #     y = "visit_group",
        #     group = "inkluderad"
        # )
# 
        # map <- map$server(
        #     "output",
        #     dat_synopsis,
        #     geo,
        #     x = "lan",
        #     y = "visit_group",
        #     group = "inkluderad"
        # )
# 
        # output$table <- rtbl$renderReactable(table())
# 
        # output$bar <- e4r$renderEcharts4r(bar())
# 
        # output$map <- e4r$renderEcharts4r(map())
    })
}
