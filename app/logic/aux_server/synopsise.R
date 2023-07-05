box::use(
    dp = dplyr,
    sh = shiny,
    rl = rlang,
    ts = tidyselect,
    stats,
    magrittr[`%>%`],
)

box::use(
    # TODO integrate necessary srqlib functions into project or make a package?
    srqlib / srqprep,
)

#' @export
synopsise <- function(.data, .fn, .var = "outcome", .by, riket = TRUE, ...) {
    dots <- rl$quos(...)

    if (riket) {
        .data <- srqprep$prep_riket(.data, .var, .fn, .by, !!!dots)
    }

    out <- .data %>%
        dp$summarise(
            outcome = .fn(.data[[.var]], !!!dots),
            missing = sum(is.na(.data[[.var]])),
            nonmissing = sum(!is.na(.data[[.var]])),
            .by = ts$all_of(.by)
        )

    digits <- dp$case_when(
        max(out[["outcome"]]) <= 10 ~ 2,
        max(out[["outcome"]]) %>% dp$between(10.01, 99.99) ~ 1,
        max(out[["outcome"]]) >= 100 ~ 0,
    )

    out %>%
        dp$mutate(
            outcome = round(.data[["outcome"]], digits)
        )
}
