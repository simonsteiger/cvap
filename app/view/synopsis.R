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
server <- function(id, .data, ...) { # pass the data of the current vap
    sh$moduleServer(id, function(input, output, session) {
        stopifnot(sh$is.reactive(.data))

        dots <- rl$list2(...)

        out <- sh$reactive(
            .data() %>%
                dp$mutate(dp$across(ts$where(lub$is.Date), \(x) lub$floor_date(x, "years"))) %>%
                ase$synopsise(...)
        )

        fct_var <- sh$reactive(
            out() %>%
                dp$select(ts$where(is.factor)) %>%
                colnames()
        )

        sh$reactive(
            srqprep$prep_custom_order(
                out(),
                .reorder = "lan",
                .by = dots$.var,
                .data[[fct_var()]] == levels(.data[[fct_var()]])[2]
            )
        )
    })
}
