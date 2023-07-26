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
    shj = shinyjs,
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

title <- aui$navbox_data$tag[[4]][[1]]

#' @export
ui <- function(id, data) {
    ns <- sh$NS(id)

    inputs <- sh$tagList(
        aui$inp_daterange(sh$NS(ns("input"), "inkluderad"), "Välj tidsfönster för inklusionsdatum"),
        aui$inp_radio_sex(sh$NS(ns("input"), "kon")),
        aui$inp_slider_age(sh$NS(ns("input"), "alder")),
        aui$inp_picker_lan(sh$NS(ns("input"), "lan"), unique(data$lan)),
        sift$ui(ns("input")) # outputs error when no lan selected
    )

    sh$tagList(
        aui$container_fluid(
            aui$row(
                class_row = "row m-4 d-flex justify-content-center align-items-center",
                left = sh$div(aui$btn_return(ns("return"))),
                center = sh$div(class = "fs-1 h-font text-center", title)
            ),
            aui$row_sidebar(
                sidebar = sh$div(
                    aui$sidebar_filter(
                        ns("go_input"), ns("overview"),
                        inputs,
                        modal_summary = sh$htmlOutput(sh$NS(ns("input"), "n_cases"))
                    ),
                    warning$ui(ns("warning"))
                ),
                main = sh$tagList(
                    bar$ui(ns("output")),
                    map$ui(ns("output")),
                    aui$card(
                        header = sh$div(class = "py-card-header", "Sammanfattning"),
                        body = sh$textOutput(ns("text"))
                    ),
                    table$ui(ns("output"))
                )
            )
        )
    )
}

#' @export
server <- function(id, access_page, data, geo, summary) {
    sh$moduleServer(id, function(input, output, session) {
        ase$obs_return(input)

        out_stash <- sh$bindEvent(
            stash$server("input", title, "Andel med uppföljningsbesök"),
            list(input$go_input, access_page)
        )

        sh$observe({
            shj$toggleState("go_input", nrow(sifted()) > 0)
        })

        out_icons <- sh$bindEvent(
            overview$server("input"),
            list(input$go_input, access_page)
        )

        sifted <- sift$server("input", sh$reactive(data), "visit_group")

        sum_synopsis <- sh$bindEvent(
            synopsis$server(
                "summary",
                sifted,
                .fn = mean,
                .var = "visit_group",
                .by = c("lan", "timestamp"),
                na.rm = TRUE
            ),
            list(input$go_input, access_page)
        )

        sum_warn <- warning$server(
            "warning",
            sum_synopsis
        )

        sum_sort <- sort$server(
            "output",
            sum_warn,
            group = "timestamp"
        )

        table$server(
            "output",
            sum_sort,
            stash = out_stash,
            arrange = c("lan", "timestamp")
        )

        bar$server(
            "output",
            sum_sort,
            stash = out_stash,
            group = "timestamp",
            text = title,
            format = "percent",
            timeline = TRUE
        )

        map$server(
            id = "output",
            .data = sum_warn,
            geo = geo,
            stash = out_stash,
            group = "timestamp",
            text = title,
            format = "percent"
        )

        output$overview <- sh$renderUI(out_icons())

        output$text <- sh$renderText(summary)
    })
}
