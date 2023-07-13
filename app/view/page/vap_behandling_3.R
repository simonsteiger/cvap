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
    app / view / output / stash,
)

title <- aui$navbox_data$tag[[2]][[3]]

#' @export
ui <- function(id, data) {
    ns <- sh$NS(id)

    inputs <- sh$tagList(
        aui$inp_date(sh$NS(ns("input"), "ongoing"), "Välj tidpunkt för pågående behandlingar"),
        aui$inp_radio_lookback(sh$NS(ns("input"), "lookback")),
        aui$inp_radio_outcome(ns("outcome"), aui$choices$das28_cdai),
        aui$inp_radio_sex(sh$NS(ns("input"), "kon")),
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

        out_stash <- sh$bindEvent(
            stash$server("input", title),
            list(input$go_input, access_page)
        )

        out_icons <- sh$bindEvent(
            overview$server("input"),
            list(input$go_input, access_page)
        )

        sieve <- sift$server("input", sh$reactive(data))

        pre_lookback <- lookback$server("input", sh$reactive(data[sieve(), ]), input$outcome)

        sum_synopsis <- sh$bindEvent(
            synopsis$server(
                "summary",
                pre_lookback,
                .fn = mean,
                .var = "outcome",
                .by = "lan",
                na.rm = TRUE
            ),
            list(input$go_input, access_page)
        )

        sum_sort <- sort$server(
            "output",
            sum_synopsis
        )

        table$server(
            "output",
            sum_sort,
            arrange = "lan"
        )

        bar$server(
            "output",
            sum_sort,
            stash = out_stash,
            text = title
        )

        map$server(
            id = "output",
            .data = sum_synopsis,
            geo = geo,
            stash = out_stash,
            group = NULL,
            text = title
        )

        output$overview <- sh$renderUI(out_icons())
    })
}
