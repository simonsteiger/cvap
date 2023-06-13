box::use(
    magrittr[`%>%`],
    sh = shiny,
    e4r = echarts4r,
    dp = dplyr,
    pr = purrr,
    lub = lubridate,
)

box::use(
    srqlib / srqcolor,
)

palette <- c(
    "#32575c",
    "#4f9e63",
    "#9fd685",
    "#c6d9bd"
)

#' @export
plot_map <- function(.data, geo, x, y, group = NULL, text = "Title", register = "Sweden") {
    stopifnot(sh$is.reactive(.data))

    if (!is.null(group)) {
        out <- sh$reactive(dp$group_by(.data(), .data[[group]]))
        lvls <- sh$reactive(unique(out()[[group]]))
    } else {
        out <- .data
    }

    title <- sh$reactive(pr$map(lvls(), \(x) {
        list(
            text = paste0(text, ", ", x), 
            subtext = paste0("Data uttagen: ", lub$today()),
            textStyle = list(color = "black", fontWeight = "bolder")
        )
    }))

    sh$reactive(
        out() %>%
            e4r$e_charts_(x, timeline = TRUE) %>%
            e4r$e_map_register(register, geo) %>%
            e4r$e_map_(y, map = "Sweden", nameProperty = "NAME_1") %>%
            e4r$e_visual_map_(y, color = palette) %>%
            e4r$e_theme("infographic") %>%
            e4r$e_toolbox_feature(feature = c("saveAsImage")) %>%
            e4r$e_timeline_opts(autoPlay = FALSE) %>%
            e4r$e_timeline_serie(title = title())
    )
}
