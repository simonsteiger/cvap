box::use(
    sh = shiny,
    shj = shinyjs,
    dp = dplyr,
)

box::use(
    aui = app / logic / aux_ui,
    ase = app / logic / aux_server,
    app / view / wrangle / sift,
    app / view / wrangle / synopsis,
    app / view / wrangle / sort,
    txt = app / view / output / text,
    tbl = app / view / output / table,
    app / view / output / bar,
    app / view / output / map,
    app / view / output / overview,
    app / view / output / warning,
    app / view / output / stash,
)

title <- aui$navbox_data$tag[[3]][[1]]

#' @export
ui <- function(id, data) {
    ns <- sh$NS(id)

    inputs <- sh$tagList(
        aui$inp_daterange(sh$NS(ns("input"), "inkluderad"), "Välj tidsfönster för inklusionsdatum"),
        aui$inp_radio_start(sh$NS(ns("input"), "start")),
        aui$inp_radio_dxcat(sh$NS(ns("input"), "dxcat"), unique(data$dxcat)),
        aui$inp_radio_sex(sh$NS(ns("input"), "kon")),
        aui$inp_slider_age(sh$NS(ns("input"), "alder")),
        aui$inp_picker_timestamp(sh$NS(ns("input"), "timestamp"), levels(data$timestamp)),
        aui$inp_picker_lan(sh$NS(ns("input"), "lan"), unique(data$lan)),
    )

    sh$tagList(
        aui$container_fluid(
            aui$head(ns("return"), title = title),
            aui$row_sidebar(
                sidebar = sh$div(
                    sift$ui(ns("input"), ns("go_input"), ns("overview"), inputs),
                    warning$ui(ns("warning"))
                ),
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
server <- function(id, access_page, data, geo, summary) {
    sh$moduleServer(id, function(input, output, session) {
        # Return to home page if return button is clicked
        ase$obs_return(input)

        # Record inputs for later use in plotting and table
        # This bridges space between input and output namespaces in an... OK way?
        out_stash <- sh$bindEvent(
            stash$server("input", title, "Andel som får diagnos inom tidsfönstret"),
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
        sifted <- sift$server("input", sh$reactive(data), "visit_group")

        # Summarise data with `.fn`, grouped by `.by`, and optionally pass `...` as args to `.fn`
        # Triggered by "go_input" and accessing the page
        sum_pre <- sh$bindEvent(
            synopsis$server(
                "summary",
                sifted,
                .fn = mean,
                .var = "visit_group",
                .by = c("lan", "lan_scb_id", "timestamp", "dxcat"), # keep dxcat for table
                na.rm = TRUE
            ),
            list(input$go_input, access_page)
        )

        # If there's a timestamp, that timestamp should be a numeric
        # During the sift step, timestamp might have to be a factor though
        # Since sift will automatically filter numerics with ranges or cutoffs
        # Instead of categorical as would be necessary here
        sum_synopsis <- sh$reactive({
            dp$mutate(sum_pre(), timestamp = as.numeric(as.character(timestamp)))
        })

        # Check sum_synopsis for läns with insufficient data
        sum_warn <- warning$server(
            "warning",
            sum_synopsis,
            out_stash
        )

        # Within läns, sort data by `group`
        sum_sort <- sort$server(
            "output",
            sum_warn,
            group = "timestamp"
        )

        # Variables by which to sort df for table and ggplot
        arrange <- c("lan", "timestamp")

        # Create table output
        tbl$server(
            "output",
            sum_sort,
            stash = out_stash,
            arrange = arrange
        )

        # Create barplot output
        bar$server(
            "output",
            sum_sort,
            stash = out_stash,
            group = "timestamp",
            text = title,
            format = "percent",
            timeline = TRUE,
            arrange = arrange
        )

        # Create map output
        map$server(
            id = "output",
            .data = sum_warn,
            geo = geo,
            stash = out_stash,
            group = "timestamp",
            text = title,
            format = "percent"
        )

        # Create text output
        txt$server("output", summary)

        # Create overview panel
        output$overview <- sh$renderUI(out_icons())
    })
}
