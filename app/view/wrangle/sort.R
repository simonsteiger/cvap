box::use(
    magrittr[`%>%`],
    sh = shiny,
    rl = rlang,
    fct = forcats,
    dp = dplyr,
    ts = tidyselect,
    lub = lubridate,
)

box::use(
    srqlib / srqprep,
)

#' @export
ui <- function(id) {
    ns <- sh$NS(id)
    sh$tagList()
}

#' @export
server <- function(id, .data, group = NULL, ...) {
    sh$moduleServer(id, function(input, output, session) {
        dots <- rl$quos(...)

        sh$reactive({
            # First check if there's sortable data, if not, do nothing
            if (nrow(.data()) > 0 && all(is.na(.data()$outcome))) { # is the nrow check necessary?
                return(.data())
            } else if (input$sort) { # TRUE = alphabetical
                return(
                    .data() %>%
                        dp$mutate(lan = as.factor(lan)) %>%
                        dp$arrange(dp$desc(lan))
                )
            } else if (is.null(group)) { # no custom reordering necessary
                return(
                    .data() %>%
                        dp$mutate(lan = fct$fct_reorder(as.factor(lan), .data()[["outcome"]])) %>%
                        dp$arrange(lan)
                )
            } else if (is.factor(.data()[[group]])) {
                fct_var <-
                    .data() %>%
                    dp$select(ts$where(is.factor)) %>%
                    colnames()

                # Order fct_var by outcome, descending
                # Pick first level = level with highest outcome values
                target_level <-
                    .data()[[fct_var]] %>%
                    droplevels(.) %>%
                    fct$fct_reorder(-.data()[["outcome"]]) %>%
                    levels(.) %>%
                    `[`(., 1)

                return(
                    srqprep$prep_custom_order(
                        .data(),
                        .reorder = "lan",
                        .by = "outcome",
                        .data[[fct_var]] == target_level
                    )
                )
            } else if (lub$is.Date(.data()[[group]])) {
                date_var <-
                    .data() %>%
                    dp$select(ts$where(lub$is.Date)) %>%
                    colnames()


                return(
                    srqprep$prep_custom_order(
                        .data(),
                        .reorder = "lan",
                        .by = "outcome",
                        .data[[date_var]] == max(.data[[date_var]])
                    )
                )
            } else if (is.numeric(.data()[[group]])) {
                group_var <- colnames(.data()) %>%
                    `[`(., . %in% group)

                return(
                    srqprep$prep_custom_order(
                        .data(),
                        .reorder = "lan",
                        .by = "outcome",
                        .data[[group_var]] == max(.data[[group_var]])
                    )
                )
            }
        })
    })
}
