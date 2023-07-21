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

        small_lans <- sh$reactive({
            sh$req(nrow(.data()) > 0)

            .data() %>%
                dp$filter(nonmissing < 10) %>%
                dp$pull(lan) %>%
                unique()
        })

        last_input_small <- sh$eventReactive(small_lans(), {
            input$exclude_low_n %||% FALSE
        })

        crit_lans <- sh$reactive({
            sh$req(nrow(.data()) > 0)

            .data() %>%
                dp$filter(nonmissing < 5) %>%
                dp$pull(lan) %>%
                unique()
        })

        # Create samplesize icons
        icons <- sh$reactive(
            sh$div(
                ase$iconostasis$samplesmall(
                    session$ns("exclude_low_n"),
                    small_lans(),
                    last_input_small()
                ),
                { # Only make crit_lan icon if there are critically small lans
                    if (length(crit_lans()) >= 1) {
                        sh$tagList(
                            sh$tags$hr(),
                            ase$iconostasis$samplecrit(
                                crit_lans()
                            )
                        )
                    }
                }
            )
        )

        output$sidebar <- sh$renderUI({
            if (length(small_lans()) >= 1) {
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
                    dp$filter(!lan %in% small_lans())
            } else {
                .data() %>%
                    dp$filter(!lan %in% crit_lans())
            }
        })
    })
}
