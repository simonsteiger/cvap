box::use(
    gg = ggplot2,
    lub = lubridate,
    pal = palettes,
    dp = dplyr,
)

box::use(
    app / logic / theme,
    app / logic / srqlib / srqcolor,
    app / logic / srqlib / srqauto,
)

plot_export_grouped <- function(.data, x, y, group, stash, scale_y) {
    gg$ggplot(.data, gg$aes(.data[[x]], .data[[y]])) +
        gg$geom_col(
            gg$aes(fill = as.factor(.data[[group]])),
            position = gg$position_dodge2(
                preserve = "single",
                padding = 0.2
            )
        ) +
        pal$scale_fill_palette_d(srqcolor$ramp(dp$n_distinct(.data[[group]]))) +
        pal$scale_color_palette_d(srqcolor$ramp(dp$n_distinct(.data[[group]]))) +
        gg$coord_flip() +
        scale_y +
        gg$xlab(NULL) +
        gg$labs(
            x = NULL,
            y = NULL, # keep settings from scale_y
            title = stash()$title,
            subtitle = stash()$subtitle,
            caption = paste0("Data uttagen: ", lub$today(), "\nwww.srq.nu")
        ) +
        gg$theme_classic() +
        theme$ggexport
}

plot_export_ungrouped <- function(.data, x, y, stash, scale_y) {
    gg$ggplot(.data, gg$aes(.data[[x]], .data[[y]])) +
        gg$geom_col(
            width = 0.5,
            fill = "#4161ab", color = "#4161ab",
            position = gg$position_dodge2(
                preserve = "single",
                padding = 0.2
            )
        ) +
        gg$coord_flip() +
        scale_y +
        gg$xlab(NULL) +
        gg$labs(
            x = NULL,
            y = NULL, # keep settings from scale_y
            title = stash()$title,
            subtitle = stash()$subtitle,
            caption = paste0("Data uttagen: ", lub$today(), "\nwww.srq.nu")
        ) +
        gg$theme_classic() +
        theme$ggexport
}

#' @export
plot_bar_export <- function(.data, x, y, group, timeline, stash) {
    scale_y <- gg$scale_y_continuous(
        name = NULL,
        labels = srqauto$guess_label_num("y", .data[[y]])
    )
    if (is.null(group)) {
        plot_export_ungrouped(.data, x, y, stash, scale_y)
    } else {
        plot_export_grouped(.data, x, y, group, stash, scale_y)
    }
}
