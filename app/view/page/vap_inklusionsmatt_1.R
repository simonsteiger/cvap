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
    app / view / output / warning,
    app / view / output / stash,
)

text <- aui$navbox_data$tag[[3]][[1]]

#' @export
ui <- function(id, data) {
    ns <- sh$NS(id)

    inputs <- sh$tagList(
        aui$inp_daterange(sh$NS(ns("input"), "inkluderad"), "Välj tidsfönster för inklusionsdatum"),
        aui$inp_radio_start(sh$NS(ns("input"), "start")),
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
                sidebar = sh$div(
                    aui$sidebar_filter(ns("go_input"), ns("overview"), inputs),
                    warning$ui(ns("warning"))
                ),
                main = sh$tagList(
                    bar$ui(ns("output")),
                    map$ui(ns("output")),
                    aui$card(
                        header = sh$div(class = "py-card-header", "Sammanfattning"),
                        body = "Sample text."
                    ),
                    table$ui(ns("output"))
                )
            )
        )
    )
}

#' @export
server <- function(id, access_page, data, geo) {
    sh$moduleServer(id, function(input, output, session) {
        ase$obs_return(input)

        out_stash <- sh$eventReactive(list(input$go_input, access_page), {
            res <- stash$server("input", "???", title)
            res()
        })

        out_icons <- sh$eventReactive(list(input$go_input, access_page), {
            overview$server("input")
        })

        pre_sift <- sh$eventReactive(list(input$go_input, access_page), {
            sieve <- sift$server("input", sh$reactive(data))
            data[sieve(), ]
        })

        sum_synopsis <- synopsis$server(
            "summary",
            pre_sift,
            .fn = mean,
            .var = "visit_group",
            .by = c("lan", "timestamp"),
            na.rm = TRUE
        )

        sum_warn <- warning$server(
            "warning",
            sum_synopsis,
            "app-vap_inklusionsmatt_1"
        )

        sum_sort <- sort$server(
            "output",
            sum_warn,
            group = "timestamp"
        )

        table$server(
            "output",
            sum_sort,
            arrange = c("lan", "timestamp")
        )

        bar$server(
            "output",
            sum_sort,
            stash = out_stash,
            group = "timestamp",
            text = text,
            format = "percent",
            timeline = TRUE
        )

        map$server(
            id = "output",
            .data = sum_warn,
            geo = geo,
            stash = out_stash,
            group = "timestamp",
            text = text
        )

        output$overview <- sh$renderUI(out_icons())
    })
}
