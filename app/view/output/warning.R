box::use(
    magrittr[`%>%`],
    dp = dplyr,
    sh = shiny,
    rl = rlang[`%||%`],
    pr = purrr,
    ht = htmltools,
)

box::use(
    aui = app / logic / aux_ui,
    ase = app / logic / aux_server,
    app / logic / swissknife / sklang[`%//%`],
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
server <- function(id, .data, stash) {
    sh$moduleServer(id, function(input, output, session) {
        stopifnot(sh$is.reactive(.data))
        stopifnot(sh$is.reactive(stash))

        small_lans <- sh$reactive({
            sh$req(nrow(.data()) > 0)
            .data() %>%
                extract_lans(nonmissing %>% dp$between(5, 9))
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
            # Get the names of läns not already included in small and crit
            remaining <- extract_lans(.data(), nonmissing > 9) %>%
                `[`(., !. %in% crit_lans() & !. %in% small_lans())

            # If many groups are selected, lans can be present in small and crit
            # To get the number of synopsis-deleted lans, we want to count every lan only once
            exclusive_small <- small_lans()[!small_lans() %in% crit_lans()]

            # Check if crit / small / remaining cover all 21 läns
            # The difference between 21 and their sum is the number of läns deleted in synopsis
            # (grouping in synopsis will delete läns with no records for given set of filters)
            n_post_synopsis <- length(crit_lans()) + length(exclusive_small) + length(remaining)
            length(stash()$input$lan) - n_post_synopsis
        })

        # Create samplesize icons
        icons <- sh$reactive({
            # Get number of entries in small_lans()
            # Check if that number is > 0
            exists_small <- length(small_lans()) > 0

            # Get number of entries in crit_lans()
            # Add that to the number of already deleted lans
            # Check if that sum is > 0
            exists_crit <- length(crit_lans()) %>% sum(., n_deleted()) > 0

            sh$div(
                if (exists_small) {
                    ase$iconostasis$samplesmall(
                        session$ns("exclude_low_n"),
                        small_lans(),
                        last_input_small()
                    )
                },
                if (exists_small && exists_crit) {
                    sh$tags$hr()
                },
                if (exists_crit) {
                    sh$tagList(
                        ase$iconostasis$samplecrit(
                            c(crit_lans(), rep("synopsis-deleted", n_deleted()))
                        )
                    )
                }
            )
        })

        output$sidebar <- sh$renderUI({
            # If any icons were created, display them
            if (length(ht$tagQuery(icons())$find("i")$selectedTags()) > 0) {
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
                    dp$mutate(outcome = ifelse(nonmissing >= 10 | lan == "Riket", outcome, NA))
                    # Try with mutate as above, filter solution below may cause issue with maps
                    # The mutate solution has the downside of the table showing NAs,
                    # and the upside of the table being "complete"
                    # Not sure if we should allow deleting Riket at this stage
                    # dp$filter(nonmissing >= 10 | lan == "Riket")
            } else {
                .data() %>%
                    dp$mutate(outcome = ifelse(nonmissing >= 5, outcome, NA))
                    # dp$filter(nonmissing >= 5)
            }
        })
    })
}
