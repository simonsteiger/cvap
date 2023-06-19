box::use(
    magrittr[`%>%`],
    rl = rlang,
    dp = dplyr,
    ts = tidyselect,
)

#' @export
squash <- function(.data, .fn, .by, ...) {
    dots <- rl$list2(...)

    .data %>%
        dp$summarise(
            outcome = .fn(!!!dots),
            .by = ts$all_of(.by)
        )
}