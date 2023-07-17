box::use(
    magrittr[`%>%`],
    sh = shiny,
    e4r = echarts4r,
    dp = dplyr,
    lub = lubridate,
    fct = forcats,
    gg = ggplot2,
    pal = palettes,
)

box::use(
    ase = app / logic / aux_server,
    aui = app / logic / aux_ui,
    app / logic / theme,
    srqlib / srqcolor,
    srqlib / srqauto,
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
                    "Infotext om stapeldiagramm"
                )
            ),
            sh$div(
                class = "d-flex justify-content-between align-items-center gap-3",
                aui$inp_toggle(ns("sort"), "Alfabetisk ordning")
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
server <- function(id, .data, stash = NULL, x = "lan", y = "outcome", group = NULL, text = "Title", format = NULL, timeline = FALSE) {
    sh$moduleServer(id, function(input, output, session) {
        stopifnot(sh$is.reactive(.data))

        time_vars <- c("visit_group", "timestamp")

        if (!is.null(group)) {
            out <- sh$reactive(
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
            )
        } else {
            out <- .data
        }

        res_interactive <- sh$reactive({
            out <- out() %>%
                e4r$e_charts_(x, timeline = timeline) %>%
                e4r$e_bar_(y) %>%
                e4r$e_tooltip() %>%
                e4r$e_legend(bottom = 0, show = !timeline) %>%
                e4r$e_toolbox_feature(feature = c("saveAsImage")) %>%
                e4r$e_title(text, paste0("Data uttagen: ", lub$today())) %>%
                e4r$e_y_axis(max = max(out()[[y]], na.rm = TRUE))

            if (!is.null(format)) {
                out %>%
                    e4r$e_y_axis(formatter = e4r$e_axis_formatter(format)) %>%
                    e4r$e_flip_coords()
            } else {
                out %>%
                    e4r$e_flip_coords()
            }
            # Flip coords only at end to avoid confusion with axis flip and target axis for formatter
        })

        res_export <- sh$reactive({
            sh$req(nrow(.data()) > 0)

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
            sh$req(nrow(.data()) > 0)
            res_interactive()
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
