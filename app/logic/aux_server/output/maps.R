box::use(
    gg = ggplot2,
    lub = lubridate,
    pal = palettes,
    e4r = echarts4r,
    magrittr[`%>%`],
)

box::use(
    app / logic / srqlib / srqcolor,
    app / logic / theme,
)

plot_map_export_ungrouped <- function(.data, y, limits, stash) {
    gg$ggplot(.data) +
        gg$geom_sf(gg$aes(fill = .data[[y]])) +
        pal$scale_fill_palette_c(
            srqcolor$ramp(100, "abyss"),
            na.value = "#ededed",
            limits = limits,
            breaks = c(limits, mean(limits)),
        ) +
        gg$labs(
            x = NULL,
            y = NULL, # keep settings from scale_y
            title = stash()$title,
            subtitle = stash()$subtitle,
            caption = paste0("Data uttagen: ", lub$today(), "\nwww.srq.nu"),
        ) +
        gg$xlim(c(1, 33)) + # show more latitudes to give text more space
        gg$theme_void() +
        theme$ggexport
}

plot_map_export_facet <- function(.data, y, group, limits, stash) {
    nrow <- if (length(unique(.data[[group]])) > 3) 2 else 1
    gg$ggplot(.data) +
        gg$geom_sf(gg$aes(fill = .data[[y]]), linewidth = 0.1) +
        gg$facet_wrap(~ .data[[group]], nrow = nrow) +
        pal$scale_fill_palette_c(
            srqcolor$ramp(100, "abyss"),
            na.value = "#ededed",
            limits = limits,
            breaks = c(limits, mean(limits)),
        ) +
        gg$labs(
            x = NULL,
            y = NULL, # keep settings from scale_y
            title = stash()$title,
            subtitle = stash()$subtitle,
            caption = paste0("Data uttagen: ", lub$today(), "\nwww.srq.nu"),
        ) +
        gg$xlim(c(1, 33)) + # show more latitudes to give text more space
        gg$theme_void() +
        theme$ggexport
}

#' @export
plot_map_export <- function(.data, y, group, limits, stash) {
    if (is.null(group)) {
        plot_map_export_ungrouped(.data, y, limits, stash)
    } else {
        plot_map_export_facet(.data, y, group, limits, stash)
    }
}

plot_map_interactive_core <- function(.data, geo, x, y, group) {
    .data %>%
        e4r$e_charts_(x, timeline = if (!is.null(group)) TRUE else FALSE) %>%
        e4r$e_map_register("Sweden", geo$json) %>%
        e4r$e_map_(y, map = "Sweden", nameProperty = "NAME_1") %>%
        e4r$e_visual_map_(
            min = min(.data[[y]]),
            max = max(.data[[y]]),
            color = srqcolor$pal_list$abyss %>% srqcolor$reverse(),
            textStyle = list(fontFamily = "Roboto")
        ) %>%
        e4r$e_theme_custom("app/static/echarts_theme.json")
}

plot_map_interactive_timeline <- function(e, title) {
    e %>%
        e4r$e_timeline_opts(autoPlay = FALSE) %>%
        e4r$e_timeline_serie(title = title)
}

plot_map_interactive_ungrouped <- function(e, title) {
    e %>%
        e4r$e_title(
            text = title,
            paste0("Data uttagen: ", lub$today()),
            textStyle = list(fontFamily = "Commissioner"),
            subtextStyle = list(fontFamily = "Roboto")
        )
}

#' @export
plot_map_interactive <- function(.data, geo, x, y, group, title) {
    core <- plot_map_interactive_core(.data, geo, x, y, group)
    if (is.null(group)) {
        plot_map_interactive_ungrouped(core, title)
    } else {
        plot_map_interactive_timeline(core, title)
    }
}
