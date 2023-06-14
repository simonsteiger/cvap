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
    rl = rlang,
)

box::use(
    swissknife / skwrangle[`%per100k%`],
    aui = app / logic / aux_ui,
    ase = app / logic / aux_server,
    app / view / wrangle / sift,
    app / view / wrangle / squash,
    app / view / wrangle / synopsis,
    app / view / output / table,
    app / view / output / bar,
    app / view / output / map,
    app / view / output / overview,
)

text <- aui$navbox_data[[1]][[2]]

#' @export
ui <- function(id) {
    ns <- sh$NS(id)

    inputs <- sh$tagList(
        aui$inp_daterange(sh$NS(ns("sift"), "ordinerat"), "Välj tidsfönster för inklusionsdatum"),
        aui$inp_radio_sex(sh$NS(ns("sift"), "kon"))
    )

    sh$tagList(
        aui$container_fluid(
            aui$row(
                class_row = "m-4 d-flex align-items-center",
                left = sh$div(aui$btn_return(ns("return"))),
                center = aui$head()
            ),
            aui$row(
                colwidths = c(2, 10, 0),
                left = bsl$card(
                    class = "my-3",
                    height = "650px",
                    bsl$card_header(
                        sh$div(
                            class = "d-flex justify-content-between align-items-center",
                            "Översikt",
                            aui$btn_modal(ns("go"), "Filtermeny", "Bekräfta", "Stäng", inputs)
                        )
                    ),
                    # bsl$card_body(sh$htmlOutput(ns("overview")))
                ),
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
server <- function(id, access_page, data, geo) {
    sh$moduleServer(id, function(input, output, session) {
        ase$obs_return(input)

        # icons <- sh$eventReactive(list(input$go, access_page), {
        #     overview$server("sift")
        # })
# 
        # sifted <- sh$eventReactive(list(input$go, access_page), {
        #     sieve <- sift$server("sift", sh$reactive(data))
        #     data[sieve(), ]
        # })
# 
        # dat_squash <- squash$server(
        #     "sqash",
        #     sifted,
        #     .name = "n",
        #     .fn = dp$n,
        #     .by = c("lan", "population", "ordinerat")
        # )
# 
        # dat_synopsis <- synopsis$server(
        #     "summary",
        #     dat_squash,
        #     group = "ordinerat",
        #     .fn = `%per100k%`,
        #     .var = "n",
        #     .by = c("lan", "ordinerat", "population"),
        #     riket = FALSE,
        #     .data[["population"]]
        # )
# 
        # table <- table$server(
        #     "output",
        #     dat_synopsis,
        #     arrange = c("lan", "ordinerat")
        # )
# 
        # bar <- bar$server(
        #     "output",
        #     dat_synopsis,
        #     x = "lan",
        #     y = "n",
        #     group = "ordinerat",
        #     text = text
        # )
# 
        # map <- map$server(
        #     "output",
        #     dat_synopsis,
        #     geo,
        #     x = "lan",
        #     y = "n",
        #     group = "ordinerat"
        # )

        # output$overview <- sh$renderUI(icons())
# 
        # output$table <- rtbl$renderReactable(table())
# 
        # output$bar <- e4r$renderEcharts4r(bar())

        # output$map <- e4r$renderEcharts4r(map())
    })
}
