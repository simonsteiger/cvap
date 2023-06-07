box::use(
    magrittr[`%>%`],
    e4r = echarts4r,
    dp = dplyr,
)

#' @export
plot_map <- function(.data, geo, x, y, group = NULL, register = "Sweden") {
    if (!is.null(group)) {
        .data <- dp$group_by(.data, .data[[group]])
    }

    .data %>%
        e4r$e_charts_(x, timeline = TRUE) %>%
        e4r$e_map_register(register, geo) %>%
        e4r$e_map_(y, map = "Sweden", nameProperty = "NAME_1") %>%
        e4r$e_visual_map_(y) %>%
        e4r$e_theme("infographic") %>%
        e4r$e_timeline_opts(autoPlay = FALSE) #%>%
        #e4r$e_timeline_serie()
}
