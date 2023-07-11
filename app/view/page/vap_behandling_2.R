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
    app / view / wrangle / ongoing,
    app / view / output / table,
    app / view / output / bar,
    app / view / output / map,
    app / view / output / overview,
    app / view / output / stash,
)

title <- aui$navbox_data$tag[[2]][[2]]

#' @export
ui <- function(id, data) {
    ns <- sh$NS(id)

    inputs <- sh$tagList(
        aui$inp_daterange(sh$NS(ns("input"), "ongoing"), "Välj tidpunkt för pågående behandlingar"),
        aui$inp_radio_sex(sh$NS(ns("input"), "kon")),
        # aui$inp_slider_age(sh$NS(ns("input"), "alder")),
        aui$inp_picker_lan(sh$NS(ns("input"), "lan"), unique(data$lan))
    )

    sh$tagList(
        aui$container_fluid(
            aui$row(
                class_row = "row m-4 d-flex justify-content-center align-items-center",
                left = sh$div(aui$btn_return(ns("return"))),
                center = aui$head(text = title)
            ),
            aui$row_sidebar(
                sidebar = aui$sidebar_filter(ns("go_input"), ns("overview"), inputs),
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
            res <- stash$server("input", "Antal pågående behandlingar per 100 000 invånare", title)
            res()
        })

        out_icons <- sh$eventReactive(list(input$go_input, access_page), {
            overview$server("input")
        })

        pre_sift <- sh$eventReactive(list(input$go_input, access_page), {
            sieve <- sift$server("input", sh$reactive(data))
            data[sieve(), ]
        })

        pre_ongoing <- sh$eventReactive(list(input$go_input, access_page), {
            res <- ongoing$server("input", pre_sift)
            res()
        })

        sum_squash <- squash$server(
            "summary",
            pre_ongoing,
            .fn = dp$n,
            .by = c("lan", "ongoing_timestamp", "population")
        )

        sum_synopsis <- synopsis$server(
            "summary",
            sum_squash,
            .fn = `%per100k%`,
            .var = "outcome",
            .by = c("lan", "ongoing_timestamp", "population"),
            riket = FALSE,
            .data[["population"]]
        )

        sum_sort <- sort$server(
            "output",
            sum_synopsis,
            group = "ongoing_timestamp"
        )

        table$server(
            "output",
            sum_sort,
            arrange = c("lan", "ongoing_timestamp")
        )

        bar$server(
            "output",
            sum_sort,
            stash = out_stash,
            group = "ongoing_timestamp",
            text = title
        )

        map$server(
                id = "output",
                .data = sum_synopsis,
                geo = geo,
                stash = out_stash,
                group = "ongoing_timestamp",
                text = title
            )

        output$overview <- sh$renderUI(out_icons())
    })
}
