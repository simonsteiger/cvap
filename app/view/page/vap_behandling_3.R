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
    app / view / wrangle / sort,
    app / view / wrangle / lookback,
    app / view / output / table,
    app / view / output / bar,
    app / view / output / map,
    app / view / output / overview,
)

worker <- ase$initialize_worker()

text <- aui$navbox_data$tag[[1]][[2]]

#' @export
ui <- function(id, data) {
    ns <- sh$NS(id)

    inputs <- sh$tagList(
        aui$inp_date(sh$NS(ns("input"), "ongoing"), "Välj tidpunkt för pågående behandlingar"),
        aui$inp_radio_lookback(sh$NS(ns("input"), "lookback")),
        aui$inp_picker_outcome(ns("outcome")), # visible across entire module
        aui$inp_radio_sex(sh$NS(ns("input"), "kon")),
        # aui$inp_slider_age(sh$NS(ns("input"), "alder")),
        aui$inp_picker_lan(sh$NS(ns("input"), "lan"), unique(data$lan))
    )

    sh$tagList(
        aui$container_fluid(
            aui$row(
                class_row = "row m-4 d-flex justify-content-center align-items-center",
                left = sh$div(aui$btn_return(ns("return"))),
                center = aui$head(text = text)
            ),
            aui$row_sidebar(
                sidebar = aui$sidebar(
                    header = aui$btn_modal(
                        ns("go"),
                        label = sh$tagList(sh$icon("filter"), "Anpassa"),
                        modal_title = "Filtermeny",
                        footer_confirm = "Bekräfta",
                        footer_dismiss = "Avbryt",
                        inputs
                    ),
                    body = sh$htmlOutput(ns("overview"))
                ),
                main = sh$tagList(
                    aui$card(
                        header = sh$div(
                            class = "d-flex justify-content-between align-items-center",
                            sh$div(
                                class = "d-flex flex-row align-items-center",
                                "Stapeldiagramm",
                                aui$btn_modal(
                                    ns("info-stapel"),
                                    label = sh$icon("circle-info"),
                                    modal_title = "Information om stapeldiagramm",
                                    footer_confirm = NULL,
                                    footer_dismiss = NULL,
                                    class_toggle = "btn btn-transparent",
                                    "Infotext om stapeldiagramm"
                                )
                            ),
                            sh$div(
                                class = "d-flex justify-content-between align-items-center gap-3",
                                aui$inp_toggle_sort(sh$NS(ns("output"), "sort")),
                                aui$btn_modal(
                                    ns("download"),
                                    label = sh$tagList(sh$icon("download"), "Download"),
                                    modal_title = "Anpassa download",
                                    footer_confirm = NULL,
                                    footer_dismiss = NULL,
                                    "Download controls"
                                )
                            )
                        ),
                        body = e4r$echarts4rOutput(ns("bar"))
                    ),
                    aui$card(
                        header = sh$div(
                            class = "d-flex justify-content-between align-items-center",
                            sh$div(
                                class = "d-flex flex-row align-items-center",
                                "Karta",
                                aui$btn_modal(
                                    ns("info-stapel"),
                                    label = sh$icon("circle-info"),
                                    modal_title = "Information om karta",
                                    footer_confirm = NULL,
                                    footer_dismiss = NULL,
                                    class_toggle = "btn btn-transparent",
                                    "Infotext om karta"
                                )
                            ),
                            sh$div(
                                class = "d-flex justify-content-between align-items-center gap-3",
                                sh$actionButton(
                                    ns("load"),
                                    class = "hover",
                                    "Ladda karta",
                                    icon = sh$icon("hourglass-half")
                                ), # put this into map UI
                                aui$btn_modal(
                                    ns("download"),
                                    label = sh$tagList(sh$icon("download"), "Download"),
                                    modal_title = "Anpassa download",
                                    footer_confirm = NULL,
                                    footer_dismiss = NULL,
                                    "Download controls"
                                )
                            )
                        ),
                        body = e4r$echarts4rOutput(ns("map"))
                    ),
                    aui$card(
                        header = sh$div(class = "py-card-header", "Sammanfattning"),
                        body = "Sample text."
                    ),
                    aui$card(
                        header = sh$div(
                            class = "d-flex flex-row align-items-center",
                            "Tabell",
                            aui$btn_modal(
                                ns("info-tabell"),
                                label = sh$icon("circle-info"),
                                modal_title = "Information om tabell",
                                footer_confirm = NULL,
                                footer_dismiss = NULL,
                                class_toggle = "btn btn-transparent",
                                "Infotext om tabell"
                            )
                        ),
                        body = rtbl$reactableOutput(ns("table"))
                    )
                )
            )
        )
    )
}

# TODO give outcome generic name after synopsis, then rename for user in out_X

#' @export
server <- function(id, access_page, data, geo) {
    sh$moduleServer(id, function(input, output, session) {
        ase$obs_return(input)

        out_icons <- sh$eventReactive(list(input$go, access_page), {
            overview$server("input")
        })

        pre_sift <- sh$eventReactive(list(input$go, access_page), {
            sieve <- sift$server("input", sh$reactive(data))
            data[sieve(), ]
        })

        pre_lookback <- sh$eventReactive(list(input$go, access_page), {
            res <- lookback$server("input", pre_sift, input$outcome)
            res()
        })

        sum_synopsis <- sh$eventReactive(list(input$go, access_page), {
            res <- synopsis$server(
                "summary",
                pre_lookback,
                .fn = mean,
                .var = "outcome",
                .by = "lan",
                na.rm = TRUE
            )
            res()
        })

        sum_sort <- sort$server(
            "output",
            sum_synopsis
        )

        out_table <- table$server(
            "output",
            sum_sort,
            arrange = "lan"
        )

        out_bar <- bar$server(
            "output",
            sum_sort,
            text = text
        )

        out_map <- sh$eventReactive(input$load, {
            res <- map$server(
                id = "output",
                .data = sum_sort,
                geo = geo,
                group = NULL,
                text = text
            )
            res()
        })

        output$overview <- sh$renderUI(out_icons())

        output$table <- rtbl$renderReactable(out_table())

        output$bar <- e4r$renderEcharts4r(out_bar())

        output$map <- e4r$renderEcharts4r(out_map())
    })
}
