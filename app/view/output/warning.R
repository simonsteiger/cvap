box::use(
    magrittr[`%>%`],
    dp = dplyr,
    sh = shiny,
    rl = rlang[`%||%`],
)

box::use(
    aui = app / logic / aux_ui,
    ase = app / logic / aux_server,
    swissknife / sklang[`%//%`],
)

# Helper to get all lans with nonmissing values below `n`
extract_lans <- function(.data, cnd) {
    q <- rl$enquo(cnd)

    .data %>%
        dp$filter(!!q) %>%
        dp$pull(lan) %>%
        unique() %>%
        `[`(., !. %in% "Riket") # never mark Riket as low samplesize
}

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
                extract_lans(nonmissing %>% dp$between(5, 10))
        })

        last_input_small <- sh$eventReactive(small_lans(), {
            input$exclude_low_n %||% FALSE
        })

        crit_lans <- sh$reactive({
            sh$req(nrow(.data()) > 0)

            .data() %>%
                extract_lans(nonmissing < 5)
        })

        n_deleted <- sh$reactive({
            remaining <- extract_lans(.data(), nonmissing > 10) %>%
                `[`(., !. %in% crit_lans() & !. %in% small_lans())

            # print(paste0(crit_lans(), small_lans(), remaining, collapse = "; "))

            21 %>%
                `-`(., length(crit_lans())) %>%
                `-`(., length(small_lans())) %>%
                `-`(., length(remaining))
        })

        # Create samplesize icons
        icons <- sh$reactive(
            sh$div(
                {
                    if (length(small_lans()) > 0) {
                        ase$iconostasis$samplesmall(
                            session$ns("exclude_low_n"),
                            small_lans(),
                            last_input_small()
                        )
                    }
                },
                { # if there is an entry for small AND one for crit OR n_deleted, draw hr
                    if (length(small_lans()) > 0 & length(c(crit_lans(), n_deleted())) > 0) {
                        sh$tags$hr()
                    }
                },
                { # Only make crit_lan icon if there are critically small lans
                    if (length(c(crit_lans(), n_deleted())) > 0) {
                        sh$tagList(
                            ase$iconostasis$samplecrit(
                                c(crit_lans(), rep("synopsis-deleted", n_deleted()))
                            )
                        )
                    }
                }
            )
        )

        output$sidebar <- sh$renderUI({
            # If there's any small, crit, or deleted lans, create warning box
            if (length(c(small_lans(), crit_lans(), n_deleted())) > 0) {
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
                    dp$filter(!lan %in% crit_lans()) %>%
                    dp$filter(!lan %in% small_lans())
            } else {
                .data() %>%
                    dp$filter(!lan %in% crit_lans())
            }
        })
    })
}
