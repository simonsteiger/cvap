box::use(
    magrittr[`%>%`],
    sh = shiny,
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

        fn_unit <- switch(
            unit,
            "months" = lub$dmonths,
            "weeks" = lub$dweeks,
        )

        start <- switch(
            unit,
            "months" = lub$dmonths(2) / lub$ddays(1),
            "weeks" = lub$dweeks(10) / lub$ddays(1),
        )

        end <- switch(
            unit,
            "months" = lub$dmonths(12) / lub$ddays(1),
            "weeks" = lub$dweeks(52) / lub$ddays(1),
        )

        .data %>%
            srqprep$prep_dynamic_groups(
                .start = srqdate$no_limit,
                .end = lub$today() - fn_unit(input$regroup),
                .start_var = "min_ins_ord",
                .end_var = "datum",
                diff >= end & diff <= start ~ TRUE, # STYLE start end nomenclature confusing (?)
                .default = FALSE
            ) %>%
            dp$arrange(patientkod, dp$desc(visit_group)) %>%
            dp$distinct(patientkod, .keep_all = TRUE)
    })
}
