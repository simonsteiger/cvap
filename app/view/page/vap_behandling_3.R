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
    shj = shinyjs,
)

box::use(
    swissknife / skwrangle[`%per100k%`], # IMPORT skwrangle
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
    app / view / output / warning,
)

title <- aui$navbox_data$tag[[2]][[3]]

#' @export
ui <- function(id, data) {
    ns <- sh$NS(id)

    inputs <- sh$tagList(
        aui$inp_date(sh$NS(ns("input"), "ongoing"), "Välj tidpunkt för pågående behandlingar"),
        aui$inp_radio_lookback(sh$NS(ns("input"), "lookback")),
        aui$inp_radio_outcome(sh$NS(ns("input"), "outcome"), aui$choices$das28_cdai),
        aui$inp_radio_sex(sh$NS(ns("input"), "kon")),
        aui$inp_picker_lan(sh$NS(ns("input"), "lan"), unique(data$lan)),
        sift$ui(ns("input")) # outputs error when no lan selected
    )

    sh$tagList(
        aui$container_fluid(
            aui$head(ns("return"), title = title),
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
            stash$server("input", title),
            list(input$go_input, access_page)
        )

        sh$observe({
            shj$toggleState("go_input", nrow(sifted()) > 0)
        })

        out_icons <- sh$bindEvent(
            overview$server("input"),
            list(input$go_input, access_page)
        )

        sifted <- sift$server("input", sh$reactive(data))

        sum_synopsis <- sh$bindEvent(
            synopsis$server(
                "input", # has input$outcome that must be visible
                sifted,
                .fn = mean,
                .by = "lan",
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
            sum_warn
        )

        table$server(
            "output",
            sum_sort,
            stash = out_stash,
            arrange = "lan"
        )

        bar$server(
            "output",
            sum_sort,
            stash = out_stash,
            text = title,
            timeline = FALSE, # FIX This is ignored or overridden, huh!
            format = "percent"
        )

        map$server(
            id = "output",
            .data = sum_warn,
            geo = geo,
            stash = out_stash,
            text = title,
            format = "percent"
        )

        output$overview <- sh$renderUI(out_icons())

        output$text <- sh$renderText(summary)
    })
}
