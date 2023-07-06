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

text <- aui$navbox_data$tag[[2]][[4]]

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
            .fn = stats$median,
            .var = "patientens_globala",
            .by = c("lan", "visit_group"),
            na.rm = TRUE
        )

        sum_sort <- sort$server(
            "output",
            sum_synopsis,
            group = "visit_group"
        )

        table$server(
            "output",
            sum_sort,
            arrange = c("lan", "visit_group")
        )

        bar$server(
            "output",
            sum_sort,
            group = "visit_group",
            text = text
        )

        map$server(
            id = "output",
            .data = sum_sort,
            geo = geo,
            group = "visit_group",
            text = text
        )

        output$overview <- sh$renderUI(out_icons())
    })
}
