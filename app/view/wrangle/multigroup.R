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
server <- function(id, .data, unit, start) {
    sh$moduleServer(id, function(input, output, session) {
        stopifnot(sh$is.reactive(.data))

        # TODO if the input to dxcat is tidig_SpA/PsA/AS, use different timescale

        unit_fn <- switch(unit,
            "months" = lub$dmonths,
            "weeks" = lub$dweeks,
        )

        unit_seq <- switch(unit,
            "months" = seq(from = 4, to = 12, by = 2),
            "weeks" = seq(from = 20, to = 60, by = 10)
        )

        unit_min <- switch(unit,
            "months" = lub$dmonths(2) / lub$ddays(1),
            "weeks" = lub$dweeks(10) / lub$ddays(1),
        )

        sh$reactive({
            out <- .data() %>%
                dp$mutate(diff = as.numeric(datum - .data[[start]]))

            pr$map(unit_seq, \(t) {
                t_days <- unit_fn(t) / lub$ddays(1)
                out %>%
                    dp$filter(.data[[start]] <= lub$today() - t_days) %>%
                    dp$mutate(
                        visit_group = ifelse(diff <= t_days & diff >= unit_min, TRUE, FALSE),
                        timestamp_group = factor(t)
                    ) %>%
                    dp$arrange(patientkod, dp$desc(visit_group)) %>%
                    dp$distinct(patientkod, .keep_all = TRUE)
            }) %>%
                pr$list_rbind()
        })
    })
}
