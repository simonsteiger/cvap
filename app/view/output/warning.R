box::use(
    magrittr[`%>%`],
    dp = dplyr,
    sh = shiny,
    rl = rlang[`%||%`],
)

box::use(
    aui = app / logic / aux_ui,
    ase = app / logic / aux_server,
)

#' @export
ui <- function(id) {
    ns <- sh$NS(id)
    sh$tagList(sh$htmlOutput(ns("sidebar")))
}

#' @export
server <- function(id, .data) {
    sh$moduleServer(id, function(input, output, session) {
        stopifnot(sh$is.reactive(.data))

        low_n_lans <- sh$reactive({
            sh$req(nrow(.data()) > 0)

            .data() %>%
                dp$filter(nonmissing < 10) %>%
                dp$pull(lan) %>%
                unique()
        })

        last_input <- sh$eventReactive(low_n_lans(), {
            input$exclude_low_n %||% FALSE
        })

        # Create samplesize icon
        icons <- sh$reactive(
            ase$iconostasis$samplesize(
                session$ns("exclude_low_n"),
                low_n_lans(),
                last_input()
            )
        )

        output$sidebar <- sh$renderUI({
            if (length(low_n_lans()) >= 1) {
                aui$sidebar(
                    header = sh$div(class = "py-card-header", "Varningar"),
                    body = icons()
                )
            } else {
                NULL
            }
        })

        sh$reactive({
            if (input$exclude_low_n %||% FALSE) {
                .data() %>%
                    dp$filter(!lan %in% low_n_lans())
            } else {
                .data()
            }
        })
    })
}
