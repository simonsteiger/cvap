box::use(
    magrittr[`%>%`],
    sh = shiny,
    e4r = echarts4r,
    dp = dplyr,
    ts = tidyselect,
)

#' @export
plot_bar <- function(.data, x, y, group = NULL) {
    stopifnot(sh$is.reactive(.data))

    if (!is.null(group)) {
        out <- sh$reactive(dp$group_by(.data(), .data[[group]]))
    } else {
        out <- .data
    }

    sh$reactive(
        out() %>%
            e4r$e_charts_(x) %>%
            e4r$e_bar_(y) %>%
            e4r$e_flip_coords() %>%
            e4r$e_tooltip()
    )
}
