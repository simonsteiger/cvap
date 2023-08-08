box::use(
    gg = ggplot2,
    lub = lubridate,
    pal = palettes,
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
    nrow <- if (length(.data[[group]]) > 3) 2 else 1
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
