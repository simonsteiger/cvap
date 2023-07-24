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
    app / view / wrangle / ongoing,
    app / view / output / table,
    app / view / output / bar,
    app / view / output / map,
    app / view / output / overview,
    app / view / output / stash,
)

title <- aui$navbox_data$tag[[2]][[1]]

#' @export
ui <- function(id, data) {
    ns <- sh$NS(id)

    inputs <- sh$tagList(
        aui$inp_date(sh$NS(ns("input"), "ongoing"), "Välj tidpunkt för pågående behandlingar"),
        aui$inp_radio_sex(sh$NS(ns("input"), "kon")),
        # aui$inp_slider_age(sh$NS(ns("input"), "alder")),
        aui$inp_picker_dxcat(sh$NS(ns("input"), "dxcat"), levels(data$dxcat)),
        aui$inp_picker_lan(sh$NS(ns("input"), "lan"), unique(data$lan)),
        sift$ui(ns("input")) # outputs error when no lan selected
    )

    sh$tagList(
        aui$container_fluid(
            aui$row(
                class_row = "row m-4 d-flex justify-content-center align-items-center",
                left = sh$div(aui$btn_return(ns("return"))),
                center = aui$head(text = title)
            ),
            aui$row_sidebar(
                sidebar = aui$sidebar_filter(
                    ns("go_input"), ns("overview"),
                    inputs,
                    modal_summary = sh$htmlOutput(sh$NS(ns("input"), "n_cases"))
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
            stash$server("input", title, "Antal pågående behandlingar"),
            list(input$go_input, access_page)
        )

        sh$observe({
            cnd <- nrow(sifted()) > 0 & !is.null(out_stash()$input$lan)
            shj$toggleState("go_input", cnd)
        })

        out_icons <- sh$bindEvent(
            overview$server("input"),
            list(input$go_input, access_page)
        )

        sifted <- sift$server("input", sh$reactive(data), "patientkod")

        sum_squash <- sh$bindEvent(
            squash$server(
                "summary",
                sifted,
                .fn = dp$n,
                .by = c("lan", "dxcat", "ongoing_timestamp")
            ),
            list(input$go_input, access_page)
        )

        sum_sort <- sort$server(
            "output",
            sum_squash,
            group = "dxcat"
        )

        table$server(
            "output",
            sum_sort,
            stash = out_stash,
            arrange = c("lan", "dxcat")
        )

        bar$server(
            "output",
            sum_sort,
            stash = out_stash,
            group = "dxcat",
            text = title
        )

        map$server(
            id = "output",
            .data = sum_squash,
            geo = geo,
            stash = out_stash,
            group = "dxcat",
            text = title
        )

        output$overview <- sh$renderUI(out_icons())

        output$text <- sh$renderText(summary)
    })
}
