box::use(
    magrittr[`%>%`],
    fst,
    stats,
    sh = shiny,
    dp = dplyr,
    ts = tidyselect,
    rl = rlang,
    gg = ggplot2,
    ts = tidyselect,
    lub = lubridate,
    fct = forcats,
    str = stringr,
)

box::use(
    ase = app / logic / aux_server,
    srqlib / srqprep,
)

#' @export
ui <- function(id, ...) {
    ns <- sh$NS(id)
    sh$tagList(
        ...
    )
}

#' @export
server <- function(id, .data, group, ...) {
    sh$moduleServer(id, function(input, output, session) {
        stopifnot(sh$is.reactive(.data))

        dots <- rl$quos(...)

        out <- sh$reactive(
            # If data is preprocessed, no need to floor_date
            if (str$str_detect(deparse(substitute(.data)), "^dat_.+")) {
                .data()
            } else {
                .data() %>%
                    dp$mutate(dp$across(ts$where(lub$is.Date), \(x) lub$floor_date(x, "years")))
            }
        )

        syn <- sh$reactive(
            out() %>%
                ase$synopsise(...)
        )

        sh$reactive(
            if (is.factor(.data()[[group]])) {
                fct_var <- sh$reactive(
                    syn() %>%
                        dp$select(ts$where(is.factor)) %>%
                        colnames()
                )

                return(
                    srqprep$prep_custom_order(
                        syn(),
                        .reorder = "lan",
                        .by = rl$quo_get_expr(dots$.var),
                        .data[[fct_var()]] == levels(.data[[fct_var()]])[2]
                    )
                )
            } else if (lub$is.Date(.data()[[group]])) {
                date_var <- sh$reactive(
                    syn() %>%
                        dp$select(ts$where(lub$is.Date)) %>%
                        colnames()
                )

                return(
                    srqprep$prep_custom_order(
                        syn(),
                        .reorder = "lan",
                        .by = rl$quo_get_expr(dots$.var),
                        .data[[date_var()]] == max(.data[[date_var()]])
                    )
                )
            }
        )
    })
}
