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

title <- aui$navbox_data$tag[[4]][[2]]

#' @export
ui <- function(id, data) {
    ns <- sh$NS(id)

    inputs <- sh$tagList(
        aui$inp_daterange(sh$NS(ns("input"), "year"), "Välj tidsfönster"),
        aui$inp_picker_lan(sh$NS(ns("input"), "lan"), unique(data$lan)),
        sift$ui(ns("input")) # outputs error when no lan selected
    )

    sh$tagList(
        aui$container_fluid(
            aui$head(ns("return"), title = title),
            aui$row_sidebar(
                sidebar = sh$div(
                    aui$sidebar_filter(ns("go_input"), ns("overview"), inputs)
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

        out_icons <- sh$bindEvent(
            overview$server("input"),
            list(input$go_input, access_page)
        )

        out_stash <- sh$bindEvent(
            stash$server("input", title, "Täckningsgrad RA"),
            list(input$go_input, access_page)
        )

        sh$observe({
            shj$toggleState("go_input", nrow(sifted()) > 0)
        })

        sifted <- sift$server("input", sh$reactive(data), "lan")

        out_stash <- sh$bindEvent(
            stash$server("input", title, "Täckningsgrad RA"),
            list(input$go_input, access_page)
        )

        sum_sort <- sh$bindEvent(
            sort$server(
                "output",
                sifted,
                group = NULL
            ),
            list(input$go_input, access_page)
        )

        table$server(
            "output",
            sum_sort,
            stash = out_stash,
            arrange = c("lan", "year")
        )

        bar$server(
            "output",
            sum_sort,
            stash = out_stash,
            group = "year",
            text = title,
            timeline = TRUE,
            format = "percent"
        )

        map$server(
            id = "output",
            .data = sifted,
            stash = out_stash,
            geo = geo,
            group = "year",
            text = title,
            format = "percent"
        )

        output$overview <- sh$renderUI(out_icons())

        output$text <- sh$renderText(summary)
    })
}
