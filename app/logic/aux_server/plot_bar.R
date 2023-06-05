box::use(
    magrittr[`%>%`],
    e4r = echarts4r,
)

#' @export
plot_bar <- function(df, x, y) {
    df %>%
    e4r$e_charts(x) %>%
    e4r$e_bar(y)
}