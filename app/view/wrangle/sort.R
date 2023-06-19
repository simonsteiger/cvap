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
            if (input$sort) { # TRUE = alphabetical
                return(
                    .data() %>%
                        dp$mutate(lan = as.factor(lan)) %>%
                        dp$arrange(dp$desc(lan))
                )
            } else if (is.null(group)) { # no custom reordering necessary
                return(
                    .data() %>%
                        dp$mutate(lan = fct$fct_reorder(as.factor(lan), .data[["outcome"]])) %>%
                        dp$arrange(lan)
                )
            } else if (is.factor(.data()[[group]])) {
                fct_var <- sh$reactive(
                    .data() %>%
                        dp$select(ts$where(is.factor)) %>%
                        colnames()
                )

                return(
                    srqprep$prep_custom_order(
                        .data(),
                        .reorder = "lan",
                        .by = "outcome",
                        .data[[fct_var()]] == levels(.data[[fct_var()]])[2]
                    )
                )
            } else if (lub$is.Date(.data()[[group]])) {
                date_var <- sh$reactive(
                    .data() %>%
                        dp$select(ts$where(lub$is.Date)) %>%
                        colnames()
                )

                return(
                    srqprep$prep_custom_order(
                        .data(),
                        .reorder = "lan",
                        .by = "outcome",
                        .data[[date_var()]] == max(.data[[date_var()]])
                    )
                )
            }
        })
    })
}
