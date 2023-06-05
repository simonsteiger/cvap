box::use(
    dp = dplyr,
    sh = shiny,
    stats,
    magrittr[`%>%`],
)

box::use(
    # TODO integrate necessary srqlib functions into project or make a package?
    srqlib / srqprep,
)

#' @export
rct_summarise <- function(.data, .fn, .var, .by, riket = TRUE, ...) {
    stopifnot(sh$is.reactive(.data))

    dots <- rl$list2(...)

    if (riket) {
        .data <- srqprep$prep_riket(.data(), .var, .fn, .by, !!!dots)
    }

    sh$reactive(
        .data %>%
            dp$summarise(
                dp$across(.data[[.var]], \(x) round(.fn(x, !!!dots), 0)),
                n = dp$n(),
                .by = ts$all_of(.by)
            )
    )
}
