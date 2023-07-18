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

title <- aui$navbox_data$tag[[1]][[1]]

#' @export
ui <- function(id, data) {
    ns <- sh$NS(id)

    inputs <- sh$tagList(
        aui$inp_daterange(sh$NS(ns("input"), "inkluderad"), "Välj tidsfönster för inklusionsdatum"),
        aui$inp_radio_start(sh$NS(ns("input"), "start")),
        aui$inp_radio_sex(sh$NS(ns("input"), "kon")),
        aui$inp_slider_age(sh$NS(ns("input"), "alder")),
        aui$inp_picker_lan(sh$NS(ns("input"), "lan"), unique(data$lan)),
        sift$ui(ns("input"))
    )

    sh$tagList(
        aui$container_fluid(
            aui$row(
                class_row = "row m-4 d-flex justify-content-center align-items-center",
                left = sh$div(aui$btn_return(ns("return"))),
                center = aui$head(text = title)
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

        out_stash <- sh$bindEvent(
            stash$server("input", title, "Andel patienter som får diagnos inom 20 veckor"),
            list(input$go_input, access_page)
        )

        out_icons <- sh$bindEvent(
            overview$server("input"),
            list(input$go_input, access_page)
        )

        sieve <- sift$server("input", sh$reactive(data))

        pre_sift <- sh$reactive(data[sieve(), ])

        sum_synopsis <- sh$bindEvent(
            synopsis$server(
                "summary",
                pre_sift,
                .fn = mean,
                .var = "visit_group",
                .by = c("lan", "inkluderad"),
                na.rm = TRUE
            ),
            list(input$go_input, access_page)
        )

        sum_warn <- warning$server(
            "warning",
            sum_synopsis
        )

        sh$observe({
            if (nrow(sum_warn()) == 0) ase$error_no_data(session)
        })

        sum_sort <- sort$server(
            "output",
            sum_warn,
            group = "inkluderad"
        )

        table$server(
            "output",
            sum_sort,
            stash = out_stash,
            arrange = c("lan", "inkluderad")
        )

        bar$server(
            "output",
            sum_sort,
            stash = out_stash,
            group = "inkluderad",
            text = title,
            format = "percent"
        )

        map$server(
            id = "output",
            .data = sum_warn,
            geo = geo,
            stash = out_stash,
            group = "inkluderad",
            text = title
        )

        output$overview <- sh$renderUI(out_icons())

        output$bar <- e4r$renderEcharts4r(out_bar())
    })
}
