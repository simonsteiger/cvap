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
        # Return to home page if return button is clicked
        ase$obs_return(input)

        # Record inputs for later use in plotting and table
        # This bridges space between input and output namespaces in an... OK way?
        out_stash <- sh$bindEvent(
            stash$server("input", title, "Täckningsgrad RA"),
            list(input$go_input, access_page)
        )

        # Create icons for input summary on main VAP page
        # This is only updated whenever a page is accessed or "go_input" is clicked
        out_icons <- sh$bindEvent(
            overview$server("input"),
            list(input$go_input, access_page)
        )

        # The assertion used in the other VAPs is not useful here
        # Input data frame consists of summarised data
        # The check necessary here is only whether there are any rows,
        # as each row represents a län already
        sh$observe({
            shj$toggleState("go_input", nrow(sifted()) > 0)
        })

        # Create filtered data
        sifted <- sift$server("input", sh$reactive(data), "lan")

        # Within läns, sort data by `group`
        # Triggered by "go_input" and accessing the page
        sum_sort <- sh$bindEvent(
            sort$server(
                "output",
                sifted,
                group = NULL
            ),
            list(input$go_input, access_page)
        )

        # Create table output
        table$server(
            "output",
            sum_sort,
            stash = out_stash,
            arrange = c("lan", "year")
        )

        # Create barplot output
        bar$server(
            "output",
            sum_sort,
            stash = out_stash,
            group = "year",
            text = title,
            timeline = TRUE,
            format = "percent"
        )

        # Create map output
        map$server(
            id = "output",
            .data = sifted,
            stash = out_stash,
            geo = geo,
            group = "year",
            text = title,
            format = "percent"
        )

        # Create overview panel
        output$overview <- sh$renderUI(out_icons())

        # Create summary text output
        output$text <- sh$renderText(summary)
    })
}
