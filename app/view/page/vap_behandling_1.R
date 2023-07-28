box::use(
    sh = shiny,
    dp = dplyr,
    shj = shinyjs,
)

box::use(
    aui = app / logic / aux_ui,
    ase = app / logic / aux_server,
    app / view / wrangle / sift,
    app / view / wrangle / squash,
    app / view / wrangle / synopsis,
    app / view / wrangle / sort,
    app / view / wrangle / ongoing,
    tbl = app / view / output / table,
    txt = app / view / output / text,
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
server <- function(id, access_page, data, geo, summary) {
    sh$moduleServer(id, function(input, output, session) {
        # Return to home page if return button is clicked
        ase$obs_return(input)

        # Record inputs for later use in plotting and table
        # This bridges space between input and output namespaces in an... OK way?
        out_stash <- sh$bindEvent(
            stash$server("input", title, "Antal pågående behandlingar"),
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

        # Perform a column-independent operation to summarise data, such as dp$n
        # Triggered by "go_input" and accessing the page
        sum_squash <- sh$bindEvent(
            squash$server(
                "summary",
                sifted,
                .fn = dp$n,
                .by = c("lan", "dxcat", "ongoing_timestamp")
            ),
            list(input$go_input, access_page)
        )

        # Within läns, sort data by `group`
        sum_sort <- sort$server(
            "output",
            sum_squash,
            group = "dxcat"
        )

        # Create table output
        tbl$server(
            "output",
            sum_sort,
            stash = out_stash,
            arrange = c("lan", "dxcat")
        )

        # Create barplot output
        bar$server(
            "output",
            sum_sort,
            stash = out_stash,
            group = "dxcat",
            text = title
        )

        # Create map output
        map$server(
            id = "output",
            .data = sum_squash,
            geo = geo,
            stash = out_stash,
            group = "dxcat",
            text = title
        )

        # Create text output
        txt$server("output", summary)

        # Create overview panel
        output$overview <- sh$renderUI(out_icons())
    })
}
