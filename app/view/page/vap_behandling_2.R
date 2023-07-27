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

title <- aui$navbox_data$tag[[2]][[2]]

#' @export
ui <- function(id, data) {
    ns <- sh$NS(id)

    inputs <- sh$tagList(
        aui$inp_daterange(sh$NS(ns("input"), "ongoing"), "Välj tidpunkt för pågående behandlingar"),
        aui$inp_radio_outcome(sh$NS(ns("input"), "outcome"), c("Antal per 100 000" = "per100k", "Total antal" = "n")),
        aui$inp_radio_sex(sh$NS(ns("input"), "kon")),
        # aui$inp_slider_age(sh$NS(ns("input"), "alder")),
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
                    )
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
        # Return to home page if return button is clicked
        ase$obs_return(input)

        # Record inputs for later use in plotting and table
        # This bridges space between input and output namespaces in an... OK way?
        out_stash <- sh$bindEvent(
            stash$server("input", title),
            list(input$go_input, access_page)
        )

        # Assert that there are läns with at least 5 records (including Riket)
        # Otherwise, disable "go_input" button
        sh$observe({
            shj$toggleState("go_input", ase$check_samplesize(sifted()))
        })

        # Create icons for input summary on main VAP page
        # This is only updated whenever a page is accessed or "go_input" is clicked
        out_icons <- sh$bindEvent(
            overview$server("input"),
            list(input$go_input, access_page)
        )

        # Create filtered data
        sifted <- sift$server("input", sh$reactive(data), "patientkod")

        # User-input determines if two summarising steps or one
        # Step 1 is always a column-independent summary
        # Step 2 may convert this summary into a different measure, e.g. rate per 100k,
        # if user specified input matching if-condition.
        # Triggered by "go_input" and accessing the page
        sum_synopsis <- sh$eventReactive(list(input$go_input, access_page), {
            res <- squash$server(
                "summary",
                sifted,
                .fn = dp$n,
                .by = c("lan", "ongoing_timestamp", "population")
            )

            if (out_stash()$input$outcome == "per100k") {
                res <- synopsis$server(
                    "summary",
                    res,
                    .fn = `%per100k%`,
                    .var = "outcome",
                    .by = c("lan", "ongoing_timestamp", "population"),
                    riket = FALSE,
                    .data[["population"]]
                )
            }

            res()
        })

        # Within läns, sort data by `group`
        sum_sort <- sort$server(
            "output",
            sum_synopsis,
            group = "ongoing_timestamp"
        )

        # Create table output
        table$server(
            "output",
            sum_sort,
            stash = out_stash,
            arrange = c("lan", "ongoing_timestamp")
        )

        # Create barplot output
        bar$server(
            "output",
            sum_sort,
            stash = out_stash,
            group = "ongoing_timestamp",
            text = title
        )


        # Create map output
        map$server(
            id = "output",
            .data = sum_synopsis,
            geo = geo,
            stash = out_stash,
            group = "ongoing_timestamp",
            text = title
        )

        # Create overview panel
        output$overview <- sh$renderUI(out_icons())

        # Create summary text output
        output$text <- sh$renderText(summary)
    })
}
