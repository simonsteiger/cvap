box::use(
    magrittr[`%>%`],
    sh = shiny,
    e4r = echarts4r,
    dp = dplyr,
    lub = lubridate,
    fct = forcats,
    gg = ggplot2,
    pal = palettes,
    shj = shinyjs,
    rl = rlang[`%||%`],
)

box::use(
    ase = app / logic / aux_server,
    aui = app / logic / aux_ui,
    app / logic / theme,
    app / logic / swissknife / sklang[`%//%`],
    app / logic / srqlib / srqcolor,
    app / logic / srqlib / srqauto,
    app / logic / data / summaries[info_bar],
)

#' @export
ui <- function(id) {
    ns <- sh$NS(id)
    aui$card(
        header = sh$div(
            class = "d-flex justify-content-between align-items-center",
            sh$div(
                class = "d-flex flex-row align-items-center",
                "Stapeldiagramm",
                aui$btn_modal(
                    ns("info-stapel"),
                    label = sh$icon("circle-info"),
                    modal_title = "Information om stapeldiagramm",
                    footer_confirm = NULL,
                    footer_dismiss = NULL,
                    class_toggle = "btn btn-transparent",
                    info_bar
                )
            ),
            sh$div(
                class = "d-flex justify-content-between align-items-center gap-4",
                aui$inp_toggle(ns("decal"), "Ökad Tillgänglighet", value = TRUE),
                aui$inp_toggle(ns("sort"), "Alfabetisk ordning"),
                sh$htmlOutput(ns("inp_malniva"))
            )
        ),
        body = e4r$echarts4rOutput(ns("bar")),
        footer = sh$div(
            class = "d-flex justify-content-start align-items-center gap-3",
            sh$downloadButton(ns("exbar"), "Download", class = "hover")
        )
    )
}

#' @export
server <- function(id,
                   .data,
                   stash = NULL,
                   x = "lan",
                   y = "outcome",
                   group = NULL,
                   text = "Title",
                   format = "decimal",
                   timeline = FALSE) {
    sh$moduleServer(id, function(input, output, session) {
        stopifnot(sh$is.reactive(.data))

        time_vars <- c("visit_group", "timestamp")

        output$inp_malniva <- sh$renderUI({
            if ("Riket" %in% .data()$lan) {
                aui$inp_toggle(session$ns("malniva"), "Visa målniva")
            } else {
                NULL
            }
        })

        out <- sh$reactive({
            # Assert that group isn't NULL and there is data to work with
            if (!is.null(group) && nrow(.data()) > 0 && !all(is.na(.data()[[y]]))) {
                .data() %>%
                    dp$mutate(
                        !!group := {
                            # Only sort by y if group does not imply chronological order
                            if (!lub$is.Date(.data[[group]]) && !group %in% time_vars) {
                                as.factor(.data[[group]]) %>% fct$fct_reorder(-.data[[y]])
                            } else {
                                .data[[group]]
                            }
                        }
                    ) %>%
                    dp$group_by(.data[[group]])
            } else {
                .data()
            }
        })

        # Create echarts barplot
        res_interactive <- sh$reactive({
            limit_upper <- max(out()[[y]], na.rm = TRUE) # get limits for value axis

            out_basic <- out() %>%
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
                # TODO JS formatter needs to be adjusted to grab correct values
                e4r$e_tooltip(textStyle = list(fontFamily = "Roboto")) %>%
                e4r$e_aria(enabled = input$decal, decal = list(show = TRUE)) %>% # decal patterns
                e4r$e_theme_custom("app/static/echarts_theme.json")

            if (input$malniva %||% FALSE) {
                riket_val <- .data()$outcome[.data()$lan == "Riket"]
                riket_lwr <- (riket_val - riket_val * 0.25)
                riket_upr <- (riket_val + riket_val * 0.25)

                print(paste0(riket_lwr, riket_upr))

                out_malniva <- out_basic %>%
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
                out_malniva <- out_basic
            }

            if (!is.null(format)) {
                out_malniva %>%
                    e4r$e_y_axis(formatter = e4r$e_axis_formatter(format)) %>%
                    e4r$e_flip_coords()
            } else {
                out_malniva %>%
                    e4r$e_flip_coords()
            }
            # Flip coords last to avoid confusion with axis flip and target axis for formatter
        })

        # Create ggplot barplot for download as pdf (echarts offers only poor resolution)
        res_export <- sh$reactive({
            scale_y <- gg$scale_y_continuous(
                name = NULL,
                labels = srqauto$guess_label_num("y", out()[[y]])
            )

            gg$ggplot(out(), gg$aes(.data[[x]], .data[[y]])) +
                gg$geom_col(
                    gg$aes(fill = as.factor(.data[[group]])),
                    position = gg$position_dodge2(
                        preserve = "single",
                        padding = 0.2
                    )
                ) +
                pal$scale_fill_palette_d(srqcolor$ramp(dp$n_distinct(out()[[group]]))) +
                pal$scale_color_palette_d(srqcolor$ramp(dp$n_distinct(out()[[group]]))) +
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
        })

        output$bar <- e4r$renderEcharts4r({
            sh$req(nrow(.data()) > 0 && !all(is.na(.data()[[y]])))
            res_interactive()
        })

        # Deactivate download button if no data
        sh$observe({
            cnd <- nrow(.data()) > 0 && !all(is.na(.data()[[y]]))
            shj$toggleState("exbar", cnd)
        })

        output$exbar <- sh$downloadHandler(
            filename = function() {
                paste0(lub$today(), "_vapX_bar", ".pdf")
            },
            content = function(file) {
                gg$ggsave(file, res_export(), width = 5, height = 7)
            }
        )
    })
}
