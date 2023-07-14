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

title <- aui$navbox_data$tag[[2]][[4]]

#' @export
ui <- function(id, data) {
    ns <- sh$NS(id)

    inputs <- sh$tagList(
        aui$inp_daterange(sh$NS(ns("input"), "ordinerat"), "Välj tidsfönster för ordinerationdatum"),
        aui$inp_radio_outcome(sh$NS(ns("input"), "outcome"), aui$choices$glob_haq_smarta),
        aui$inp_radio_prep_typ(sh$NS(ns("input"), "prep_typ")),
        aui$inp_radio_sex(sh$NS(ns("input"), "kon")),
        aui$inp_slider_age(sh$NS(ns("input"), "alder")),
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
            stash$server("input", title),
            list(input$go_input, access_page)
        )

        out_icons <- sh$bindEvent(
            overview$server("input"),
            list(input$go_input, access_page)
        )

        sieve <- sift$server("input", sh$reactive(data))

        pre_sift <- sh$reactive(data[sieve(), ])

        # synopsis server is in input namespace to allow plotting user-chosen outcome
        sum_synopsis <- sh$bindEvent(
            synopsis$server(
                "input",
                pre_sift,
                .fn = stats$median,
                .by = c("lan", "visit_group"),
                na.rm = TRUE
            ),
            list(input$go_input, access_page)
        )

        sum_warn <- warning$server(
            "warning",
            sum_synopsis,
            "app-vap_behandling_4"
        )

        sum_sort <- sort$server(
            "output",
            sum_warn,
            group = "visit_group"
        )

        table$server(
            "output",
            sum_sort,
            stash = out_stash,
            arrange = c("lan", "visit_group")
        )

        bar$server(
            "output",
            sum_sort,
            stash = out_stash,
            group = "visit_group",
            text = title
        )

        map$server(
            id = "output",
            .data = sum_synopsis,
            geo = geo,
            stash = out_stash,
            group = "visit_group",
            text = title
        )

        output$overview <- sh$renderUI(out_icons())
    })
}
