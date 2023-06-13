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
    app / view / output / overview,
)

worker <- ase$initialize_worker()

text <- "Sjukdomsduration vid nydiagnosticerad RA"

#' @export
ui <- function(id, data) {
    ns <- sh$NS(id)

    inputs <- sh$tagList(
        aui$inp_daterange(sh$NS(ns("sift"), "inkluderad"), "Välj tidsfönster för inklusionsdatum"),
        aui$inp_radio_sex(sh$NS(ns("sift"), "kon")),
        aui$inp_slider_age(sh$NS(ns("sift"), "alder")),
        aui$inp_picker_lan(sh$NS(ns("sift"), "lan"), unique(data$lan))
    )

    sh$tagList(
        aui$container_fluid(
            aui$row(
                class_row = "row m-4 d-flex justify-content-center align-items-center",
                left = sh$div(aui$btn_return(ns("return"))),
                center = aui$head(text = text)
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
                            aui$btn_modal(ns("go"), "Filtermeny", "Bekräfta", "Avbryt", inputs)
                        )
                    ),
                    bsl$card_body(sh$htmlOutput(ns("overview")))
                ),
                center = sh$div(
                    aui$layout_column_wrap(
                        grid_template_columns = "3fr 2fr",
                        aui$card(
                            header = sh$div(
                                class = "d-flex justify-content-between align-items-center",
                                "Stapeldiagramm", aui$inp_toggle_sort(ns("sort"))
                            ),
                            body = e4r$echarts4rOutput(ns("bar"))
                        ),
                        aui$card(
                            header = sh$htmlOutput(ns("loader")),
                            body = e4r$echarts4rOutput(ns("map"))
                        )
                    ),
                    aui$layout_column_wrap(
                        grid_template_columns = "2fr 3fr",
                        aui$card(
                            header = "Sammanfattning",
                            body = "Sample text."
                        ),
                        aui$card(
                            header = "Tabell",
                            body = rtbl$reactableOutput(ns("table"))
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

        icons <- sh$eventReactive(list(input$go, access_page), {
            overview$server("sift")
        })

        sifted <- sh$eventReactive(list(input$go, access_page), {
            sieve <- sift$server("sift", sh$reactive(data))
            data[sieve(), ]
        })

        dat_synopsis <- synopsis$server(
            "summary",
            sifted,
            group = "inkluderad",
            .fn = mean,
            .var = "visit_group",
            .by = c("lan", "inkluderad"),
            na.rm = TRUE
        )

        table <- table$server(
            "output",
            dat_synopsis,
            arrange = c("lan", "inkluderad")
        )

        bar <- bar$server(
            "output",
            dat_synopsis,
            x = "lan",
            y = "visit_group",
            group = "inkluderad",
            text = text
        )

        output$overview <- sh$renderUI(icons())

        output$table <- rtbl$renderReactable(table())

        output$bar <- e4r$renderEcharts4r(bar())

        reactive_args <- sh$reactive({
            list(input$go, access_page)
            list(
                id = "output",
                .data = dat_synopsis,
                geo = geo,
                x = "lan",
                y = "visit_group",
                group = "inkluderad",
                text = text
            )
        })

        promise_map <- worker$run_job("map1", map$wrap, reactive_args)

        output$loader <- sh$renderUI({
            task <- promise_map()
            if (!task$resolved) {
                sh$tagList(
                    sh$div(
                        class = "d-flex justify-content-between align-items-center",
                        sh$div(sh$tags$strong("Ritar karta, var god vänta...")),
                        sh$div(
                            class = "spinner-border spinner-border-sm",
                            role = "status"
                        )
                    )
                )
            } else {
                "Karta"
            }
        })

        output$map <- e4r$renderEcharts4r({
            if (!is.null(promise_map()$result)) {
                res <- promise_map()$result
                res()
            }
        })
    })
}
