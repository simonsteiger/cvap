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
    app / view / wrangle / multigroup,
    app / view / output / table,
    app / view / output / bar,
    app / view / output / map,
    app / view / output / overview,
    app / view / output / warning,
)

text <- aui$navbox_data$tag[[4]][[2]]

#' @export
ui <- function(id, data) {
    ns <- sh$NS(id)

    inputs <- sh$tagList(
        # aui$inp_daterange(sh$NS(ns("input"), "inkluderad"), "Välj tidsfönster för inklusionsdatum"),
        # aui$inp_radio_sex(sh$NS(ns("input"), "kon")),
        # aui$inp_slider_age(sh$NS(ns("input"), "alder")),
        # aui$inp_picker_lan(sh$NS(ns("input"), "lan"), unique(data$lan))
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

        sum_sort <- sort$server(
            "output",
            sh$reactive(data),
            group = NULL
        )

        table$server(
            "output",
            sum_sort,
            arrange = c("lan", "year")
        )

        bar$server(
            "output",
            sum_sort,
            group = "year",
            text = text,
            timeline = TRUE
        )

        map$server(
            id = "output",
            .data = sum_sort,
            geo = geo,
            group = "year",
            text = text
        )
    })
}
