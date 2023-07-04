box::use(
    magrittr[`%>%`],
    sh = shiny,
    lub = lubridate,
    dp = dplyr,
    pr = purrr,
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
server <- function(id, .data, unit) {
    sh$moduleServer(id, function(input, output, session) {
        stopifnot(sh$is.reactive(.data))

        unit_fn <- switch(unit,
            "months" = lub$dmonths,
            "weeks" = lub$dweeks,
        )

        out <- .data %>%
            srqprep$prep_dynamic_groups(
                .start = srqdate$no_limit,
                .end = lub$today() - unit_fn(input$regroup),
                .start_var = "min_ins_ord",
                .end_var = "datum",
                TRUE ~ TRUE
            )

        unit_seq <- switch(unit,
            "months" = seq(from = 4, to = 12, by = 2),
            "weeks" = seq(from = 20, to = 60, by = 10)
        )

        unit_min <- switch(unit,
            "months" = lub$dmonths(2) / lub$ddays(1),
            "weeks" = lub$dweeks(10) / lub$ddays(1),
        )

        pr$map(unit_seq, \(t) {
            t_days <- unit_fn(t) / lub$ddays(1)
            out %>%
                dp$mutate(
                    visit_group = ifelse(diff <= t_days & diff >= unit_min, TRUE, FALSE),
                    timestamp_group = t
                )
        }) %>%
            pr$list_rbind()
    })
}
