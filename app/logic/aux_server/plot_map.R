box::use(
    magrittr[`%>%`],
    sh = shiny,
    e4r = echarts4r,
    dp = dplyr,
)

#' @export
plot_map <- function(.data, geo, x, y, group = NULL, register = "Sweden") {
    stopifnot(sh$is.reactive(.data))

    if (!is.null(group)) {
        out <- sh$reactive(dp$group_by(.data(), .data[[group]]))
    } else {
        out <- .data
    }

    sh$reactive(
        out() %>%
            e4r$e_charts_(x, timeline = TRUE) %>%
            e4r$e_map_register(register, geo) %>%
            e4r$e_map_(y, map = "Sweden", nameProperty = "NAME_1") %>%
            e4r$e_visual_map_(y) %>%
            e4r$e_theme("infographic") %>%
            e4r$e_timeline_opts(autoPlay = FALSE) # %>%
            # e4r$e_timeline_serie()
    )
}
