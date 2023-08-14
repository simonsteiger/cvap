box::use(
    sh = shiny,
    gg = ggplot2,
    lub = lubridate,
    pal = palettes,
    dp = dplyr,
    e4r = echarts4r,
    magrittr[`%>%`],
    rl = rlang[`%||%`],
)

box::use(
    ase = app / logic / aux_server,
    app / logic / theme,
    app / logic / srqlib / srqcolor,
    app / logic / srqlib / srqauto,
)

EXTEND_AXIS <- 0.1

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
        gg$scale_x_discrete(labels = c("Riket" = expression(bold(Riket)), parse = TRUE)) +
        scale_y +
        gg$coord_flip() +
        gg$xlab(NULL) +
        gg$labs(
            x = NULL,
            y = NULL, # keep settings from scale_y
            title = stash$title,
            subtitle = stash$subtitle,
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
        gg$scale_x_discrete(labels = c("Riket" = expression(bold(Riket)), parse = TRUE)) +
        scale_y +
        gg$coord_flip() +
        gg$xlab(NULL) +
        gg$labs(
            x = NULL,
            y = NULL, # keep settings from scale_y
            title = stash$title,
            subtitle = stash$subtitle,
            caption = paste0("Data uttagen: ", lub$today(), "\nwww.srq.nu")
        ) +
        gg$theme_classic() +
        theme$ggexport
}

#' @export
plot_bar_export <- function(.data, x, y, group, timeline, stash, input, format) {
    stopifnot(!sh$is.reactive(stash)) # must pass non-reactive

    if (format == "percent") .data[[y]] <- .data[[y]] * 100

    scale_y <- gg$scale_y_continuous(
        name = NULL,
        labels = srqauto$guess_label_num("y", .data[[y]]),
        expand = c(0, 0),
        limits = c(0, max(.data[[y]]) + max(.data[[y]]) * EXTEND_AXIS)
    )
    if (is.null(group)) {
        p <- plot_export_ungrouped(.data, x, y, stash, scale_y)
    } else {
        p <- plot_export_grouped(.data, x, y, group, stash, scale_y)
    }

    p <- mark_malniva(p, .data, input, y, type = "gg")

    if (is.null(group) || dp$n_distinct(.data[[group]]) < 3) {
        p +
            gg$geom_text(
                gg$aes(label = .data[[y]], y = .data[[y]] + 0.05),
                position = gg$position_dodge2(
                    width = 0.9,
                    preserve = "single",
                    padding = 0.2
                ),
                hjust = -0.2,
                vjust = 0.5,
                size = 2,
                color = "black"
            )
    } else {
        p
    }
}

plot_bar_interactive_core <- function(.data, input, x, y, timeline, limit_upper, text) {
    .data %>%
        e4r$e_charts_(x, timeline = timeline) %>%
        e4r$e_bar_(y) %>%
        e4r$e_legend(bottom = 0, show = !timeline) %>%
        e4r$e_title(
            text,
            paste0("Data uttagen: ", lub$today()),
            textStyle = list(fontFamily = "Commissioner"),
            subtextStyle = list(fontFamily = "Roboto")
        ) %>%
        e4r$e_y_axis_(max = limit_upper) %>%
        e4r$e_x_axis_(
            x,
            formatter = ase$format_list[["riket"]](),
            axisLabel = list(
                fontFamily = "Roboto",
                rich = list(b = list(fontWeight = "bold"))
            )
        ) %>%
        e4r$e_tooltip(textStyle = list(fontFamily = "Roboto")) %>%
        e4r$e_aria(enabled = input$decal, decal = list(show = TRUE)) %>% # decal patterns
        e4r$e_theme_custom("app/static/echarts_theme.json")
}

malniva_interactive <- function(e, .data, input, y) {
    if (input$malniva %||% FALSE) {
        riket_val <- .data[[y]][.data[["lan"]] == "Riket"]
        riket_lwr <- riket_val - riket_val * 0.25
        riket_upr <- riket_val + riket_val * 0.25

        e %>%
            e4r$e_mark_line(
                data = list(xAxis = mean(riket_lwr)),
                title = "Riket -25%",
                itemStyle = list(color = "red")
            ) %>%
            e4r$e_mark_line(
                data = list(xAxis = mean(riket_upr)),
                title = "Riket +25%",
                itemStyle = list(color = "red")
            )
    } else {
        e
    }
}

malniva_export <- function(p, .data, input, y) {
    if (input$malniva %||% FALSE) {
        riket_val <- .data[[y]][.data[["lan"]] == "Riket"]
        riket_lwr <- riket_val - riket_val * 0.25
        riket_upr <- riket_val + riket_val * 0.25

        n_lan <- length(unique(.data[["lan"]]))
        offset_top <- 0.6

        # coord flip makes this an hline even though it is vertical on final plot
        p +
            gg$geom_hline(
                yintercept = c(riket_lwr, riket_upr),
                color = "red",
                alpha = 0.8
            ) +
            gg$geom_ribbon(
                gg$aes(
                    x = seq(0, n_lan + offset_top, length.out = n_lan),
                    ymin = riket_lwr,
                    ymax = riket_upr
                ),
                fill = "red",
                alpha = 0.1
            )
    } else {
        p
    }
}


mark_malniva <- function(p, .data, input, y, type) {
    if (type == "gg") {
        malniva_export(p, .data, input, y)
    } else if (type == "e") {
        malniva_interactive(p, .data, input, y)
    } else {
        stop(paste("Unknown type", type))
    }
}

format_y <- function(e, format) {
    if (!is.null(format)) {
        e %>%
            e4r$e_y_axis(formatter = e4r$e_axis_formatter(format))
    } else {
        e
    }
}

#' @export
plot_bar_interactive <- function(.data, input, x, y, timeline, limit_upper, text, format) {
    stopifnot(!sh$is.reactive(.data))

    e <- plot_bar_interactive_core(.data, input, x, y, timeline, limit_upper, text)

    e %>%
        mark_malniva(.data, input, y, type = "e") %>%
        format_y(format) %>%
        e4r$e_flip_coords()
}
