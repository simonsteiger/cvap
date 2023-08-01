box::use(
    stats,
    sh = shiny,
    shj = shinyjs,
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

title <- aui$navbox_data$tag[[1]][[3]]

#' @export
ui <- function(id, data) {
    ns <- sh$NS(id)

    inputs <- sh$tagList(
        aui$inp_daterange(sh$NS(ns("input"), "ordinerat"), "Välj tidsfönster för ordinationdatum"),
        aui$inp_radio_sex(sh$NS(ns("input"), "kon")),
        aui$inp_slider_age(sh$NS(ns("input"), "alder")),
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
            stash$server("input", title, "Allmän hälsa"),
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
        sifted <- sift$server("input", sh$reactive(data), "patientens_globala")

        # Summarise data with `.fn`, grouped by `.by`, and optionally pass `...` as args to `.fn`
        # Triggered by "go_input" and accessing the page
        sum_synopsis <- sh$bindEvent(
            synopsis$server(
                "summary",
                sifted,
                .fn = stats$median,
                .var = "patientens_globala",
                .by = c("lan", "visit_group"),
                na.rm = TRUE
            ),
            list(input$go_input, access_page)
        )

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
            group = "visit_group"
        )

        # Create table output
        tbl$server(
            "output",
            sum_sort,
            stash = out_stash,
            arrange = c("lan", "visit_group")
        )

        # Create barplot output
        bar$server(
            "output",
            sum_sort,
            stash = out_stash,
            group = "visit_group",
            text = title
        )

        # Create map output
        map$server(
            id = "output",
            .data = sum_warn,
            geo = geo,
            stash = out_stash,
            group = "visit_group",
            text = title
        )

        # Create text output
        txt$server("output", summary)

        # Create overview panel
        output$overview <- sh$renderUI(out_icons())
    })
}
