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
    app / view / wrangle / sort,
    app / view / output / table,
    app / view / output / bar,
    app / view / output / map,
    app / view / output / overview,
)

worker <- ase$initialize_worker()

text <- aui$navbox_data$tag[[1]][[3]]

#' @export
ui <- function(id, data) {
    ns <- sh$NS(id)

    inputs <- sh$tagList(
        aui$inp_daterange(sh$NS(ns("input"), "ordinerat"), "Välj tidsfönster för ordinerationdatum"),
        aui$inp_radio_sex(sh$NS(ns("input"), "kon")),
        aui$inp_slider_age(sh$NS(ns("input"), "alder")),
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
                            aui$inp_toggle_sort(sh$NS(ns("output"), "sort"))
                        ),
                        body = e4r$echarts4rOutput(ns("bar"))
                    ),
                    aui$card(
                        header = sh$htmlOutput(ns("loader")),
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

        sum_synopsis <- synopsis$server(
            "summary",
            pre_sift,
            .fn = stats$median,
            .var = "patientens_globala",
            .by = c("lan", "visit_group"),
            na.rm = TRUE
        )

        sum_sort <- sort$server(
            "output",
            sum_synopsis,
            group = "visit_group",
            .var = "patientens_globala"
        )

        out_table <- table$server(
            "output",
            sum_sort,
            arrange = c("lan", "visit_group")
        )

        out_bar <- bar$server(
            "output",
            sum_sort,
            x = "lan",
            y = "patientens_globala",
            group = "visit_group",
            text = text
        )

        output$overview <- sh$renderUI(out_icons())

        output$table <- rtbl$renderReactable(out_table())

        output$bar <- e4r$renderEcharts4r(out_bar())

        args_map <- sh$reactive({
            list(input$go, access_page)
            list(
                id = "output",
                .data = sum_sort,
                geo = geo,
                x = "lan",
                y = "patientens_globala",
                group = "visit_group",
                text = text
            )
        })

        out_map <- worker$run_job("map3", map$wrap, args_map)

        output$loader <- sh$renderUI({
            task <- out_map()
            if (!task$resolved) {
                sh$tagList(
                    sh$div(
                        class = "d-flex justify-content-between align-items-center",
                        sh$div(
                            class = "py-card-header",
                            sh$tags$strong("Ritar karta, var god vänta...")
                        ),
                        sh$div(
                            class = "spinner-border spinner-border-sm",
                            role = "status"
                        )
                    )
                )
            } else {
                sh$div(
                    class = "d-flex flex-row align-items-center",
                    "Karta",
                    aui$btn_modal(
                        sh$NS(id, "info-karta"),
                        label = sh$icon("circle-info"),
                        modal_title = "Information om karta",
                        footer_confirm = NULL,
                        footer_dismiss = NULL,
                        class_toggle = "btn btn-transparent",
                        "Infotext om karta"
                    )
                )
            }
        })

        output$map <- e4r$renderEcharts4r({
            if (!is.null(out_map()$result)) {
                res <- out_map()$result
                res()
            }
        })
    })
}
