box::use(
    magrittr[`%>%`],
    e4r = echarts4r,
    dp = dplyr,
    ts = tidyselect,
)

#' @export
plot_bar <- function(.data, x, y, group = NULL) {
    if (!is.null(group)) {
        .data <- dp$group_by(.data, .data[[group]])
    }

    .data %>%
        e4r$e_charts_(x) %>%
        e4r$e_bar_(y) %>%
        e4r$e_flip_coords()
}
