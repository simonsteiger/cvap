box::use(
    sh = shiny,
    dp = dplyr,
    shj = shinyjs,
)

box::use(
    app / logic / swissknife / skwrangle[`%per100k%`],
    aui = app / logic / aux_ui,
    ase = app / logic / aux_server,
    app / view / wrangle / sift,
    app / view / wrangle / popjoin,
    app / view / wrangle / squash,
    app / view / wrangle / synopsis,
    app / view / wrangle / sort,
    app / view / wrangle / ongoing,
    txt = app / view / output / text,
    tbl = app / view / output / table,
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
        aui$inp_datecompare(sh$NS(ns("input"), "ongoing"), "Välj år du vill jämföra"),
        aui$inp_radio_outcome(sh$NS(ns("input"), "outcome"), c("Antal per 100 000" = "per100k", "Total antal" = "n")),
        aui$inp_radio_sex(sh$NS(ns("input"), "kon")),
        aui$inp_picker_dxcat(sh$NS(ns("input"), "dxcat"), levels(data$dxcat), multiple = FALSE),
        # aui$inp_slider_age(sh$NS(ns("input"), "alder")),
        aui$inp_picker_lan(sh$NS(ns("input"), "lan"), unique(data$lan)),
    )

    sh$tagList(
        aui$container_fluid(
            aui$head(ns("return"), title = title),
            aui$row_sidebar(
                sidebar = sift$ui(ns("input"), ns("go_input"), ns("overview"), inputs),
                main = sh$tagList(
                    bar$ui(ns("output")),
                    map$ui(ns("output")),
                    txt$ui(ns("output")),
                    tbl$ui(ns("output"))
                )
            )
        )
    )
}

#' @export
server <- function(id, access_page, data, pop, geo, summary) {
    sh$moduleServer(id, function(input, output, session) {
        # Return to home page if return button is clicked
        ase$obs_return(input)

        # Record inputs for later use in plotting and table
        # This bridges space between input and output namespaces in an... OK way?
        out_stash <- sh$bindEvent(
            stash$server("input", title, datecompare = TRUE),
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
            overview$server("input", datecompare = TRUE),
            list(input$go_input, access_page)
        )

        # Create filtered data
        sifted <- sift$server("input", sh$reactive(data), "patientkod", datecompare = TRUE)

        joined <- popjoin$server("input", sifted, sh$reactive(pop))

        # User-input determines if two summarising steps or one
        # Step 1 is always a column-independent summary
        # Step 2 may convert this summary into a different measure, e.g. rate per 100k,
        # if user specified input matching if-condition.
        # Triggered by "go_input" and accessing the page
        sum_synopsis <- sh$eventReactive(list(input$go_input, access_page), {
            res <- squash$server(
                "summary",
                joined,
                .fn = dp$n,
                .by = c("lan", "lan_scb_id", "ongoing_timestamp", "population")
            )

            if (out_stash()$input$outcome == "per100k") {
                res <- synopsis$server(
                    "summary",
                    res,
                    .fn = `%per100k%`,
                    .var = "outcome",
                    .by = c("lan", "lan_scb_id", "ongoing_timestamp", "population"),
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

        # Variables by which to sort df for table and ggplot
        arrange <- c("lan", "ongoing_timestamp")

        # Create table output
        tbl$server(
            "output",
            sum_sort,
            stash = out_stash,
            arrange = arrange,
            drop = c("missing", "nonmissing")
        )

        # Barplot output
        bar$server(
            "output",
            sum_sort,
            stash = out_stash,
            group = "ongoing_timestamp",
            text = title,
            arrange = arrange
        )

        # Map output
        map$server(
            id = "output",
            .data = sum_synopsis,
            geo = geo,
            stash = out_stash,
            group = "ongoing_timestamp",
            text = title
        )

        # Create text output
        txt$server("output", summary)

        # Create overview panel
        output$overview <- sh$renderUI(out_icons())
    })
}
