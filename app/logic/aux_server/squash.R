box::use(
    magrittr[`%>%`],
    rl = rlang,
    dp = dplyr,
    ts = tidyselect,
)

#' @export
squash <- function(.data, .fn, .by, .name, ...) {
    dots <- rl$list2(...)

    .data %>%
        dp$summarise(
            !!.name := .fn(!!!dots),
            .by = ts$all_of(.by)
        )
}