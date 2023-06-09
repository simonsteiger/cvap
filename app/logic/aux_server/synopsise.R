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
synopsise <- function(.data, .fn, .var, .by, riket = TRUE, ...) {
    dots <- rl$quos(...)

    if (riket) {
        .data <- srqprep$prep_riket(.data, .var, .fn, .by, !!!dots)
    }

    out <- .data %>%
        dp$summarise(
            dp$across(.data[[.var]], \(x) .fn(x, !!!dots)),
            nna = sum(is.na(.data[[.var]])),
            .by = ts$all_of(.by)
        )

    digits <- dp$case_when(
        max(out[[.var]]) <= 10 ~ 2,
        max(out[[.var]]) %>% dp$between(10.01, 99.99) ~ 1,
        max(out[[.var]]) >= 100 ~ 0,
    )

    out %>%
        dp$mutate(
            !!.var := round(.data[[.var]], digits)
        )
}
