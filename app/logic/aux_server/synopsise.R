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
    dots <- rl$list2(...)

    if (riket) {
        .data <- srqprep$prep_riket(.data, .var, .fn, .by, !!!dots)
    }


    .data %>%
        dp$summarise(
            dp$across(.data[[.var]], \(x) round(.fn(x, !!!dots), 0)),
            nna = sum(is.na(.data[[.var]])),
            .by = ts$all_of(.by)
        )
}
