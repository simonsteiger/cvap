box::use(
    sh = shiny,
    gg = ggplot2,
    lub = lubridate,
    pal = palettes,
    dp = dplyr,
    e4r = echarts4r,
    magrittr[`%>%`],
    rl = rlang[`%||%`],
    htw = htmlwidgets,
    sc = scales,
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
plot_bar_export <- function(.data, x, y, group, timeline, stash, input, format, custom) {
    stopifnot(!sh$is.reactive(stash)) # must pass non-reactive

    if (format == "percent") .data[[y]] <- .data[[y]] * 100
    suffix <- ifelse(format == "percent", "%", "")

    scale_y <- gg$scale_y_continuous(
        name = NULL,
        labels = sc$label_number(suffix = suffix),
        expand = c(0, 0),
        limits = c(0, max(.data[[y]]) + max(.data[[y]]) * EXTEND_AXIS)
    )
    if (is.null(group)) {
        p <- plot_export_ungrouped(.data, x, y, stash, scale_y)
    } else {
        p <- plot_export_grouped(.data, x, y, group, stash, scale_y)
    }

    p <- mark_malniva(p, .data, input, y, type = "gg", custom = custom)

    if (is.null(group) || dp$n_distinct(.data[[group]]) < 3) {
        p +
            gg$geom_text(
                gg$aes(label = paste(.data[[y]], suffix), y = .data[[y]] + 0.05),
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

left_margin <- "17%" # 17% means no hidden text before responsive breakpoint

plot_bar_interactive_core <- function(.data, input, x, y, timeline, text, format) {
    e <- .data %>%
        e4r$e_charts_(x, timeline = timeline) %>%
        e4r$e_bar_(y) %>%
        e4r$e_legend(bottom = 0, show = !timeline) %>%
        e4r$e_title(
            text,
            paste0("Data uttagen: ", lub$today()),
            textStyle = list(fontFamily = "Commissioner"),
            subtextStyle = list(fontFamily = "Roboto")
        ) %>%
        e4r$e_x_axis_(
            x,
            formatter = ase$format_list[["riket"]](),
            axisLabel = list(
                fontFamily = "Roboto",
                rich = list(b = list(fontWeight = "bold"))
            )
        ) %>%
        e4r$e_tooltip(
            textStyle = list(fontFamily = "Roboto"),
            formatter = if (format == "percent") {
                htw$JS("
                    function(params){
                        return('Län: ' + params.value[1] + '<br />Andel: ' + Math.floor(params.value[0] * 100) + '%')
                    }
                ") # formatting Län and Andel to bold would be cool, but currently bold text doesn't work well (Roboto)
            } else {
                NULL
            }
        ) %>%
        e4r$e_aria(enabled = TRUE, decal = list(show = TRUE)) %>% # decal patterns
        e4r$e_theme_custom("app/static/echarts_theme.json") %>%
        e4r$e_grid(left = left_margin) # left margin to avoid names being cut off

    if (timeline) {
        e4r$e_y_axis_(e, max = 1)
    } else {
        e
    }
}

malniva_interactive <- function(e, .data, input, y, custom) {
    if (!is.null(custom) && input$malniva %||% FALSE) {
        e %>%
            e4r$e_mark_line(
                data = list(xAxis = mean(custom)),
                title = "Målvärde",
                itemStyle = list(color = "red")
            )
    } else if (input$malniva %||% FALSE) {
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

# Custom can be a vector or a single numeric value
malniva_export <- function(p, .data, input, y, custom) {
    n_lan <- length(unique(.data[["lan"]]))
    offset_top <- 0.6

    if (!is.null(custom) && input$malniva %||% FALSE) { # specific target value or interval
        p +
            gg$geom_hline(
                yintercept = custom,
                color = "red",
                alpha = 0.8
            ) +
            gg$geom_ribbon(
                gg$aes(
                    x = seq(0, n_lan + offset_top, length.out = n_lan),
                    ymin = min(custom),
                    ymax = max(custom)
                ),
                fill = "red",
                alpha = 0.1
            )
    } else if (input$malniva %||% FALSE) { # default target interval around Riket
        riket_val <- .data[[y]][.data[["lan"]] == "Riket"]
        riket_lwr <- riket_val - riket_val * 0.25
        riket_upr <- riket_val + riket_val * 0.25

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
    } else { # no interval
        p
    }
}


mark_malniva <- function(p, .data, input, y, type, custom = NULL) {
    if (type == "gg") {
        malniva_export(p, .data, input, y, custom = custom)
    } else if (type == "e") {
        malniva_interactive(p, .data, input, y, custom = custom)
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
plot_bar_interactive <- function(.data, input, x, y, timeline, text, format, hline = NULL) {
    stopifnot(!sh$is.reactive(.data))

    e <- plot_bar_interactive_core(.data, input, x, y, timeline, text, format)

    e %>%
        mark_malniva(.data, input, y, type = "e", custom = hline) %>%
        format_y(format) %>%
        e4r$e_flip_coords()
}
