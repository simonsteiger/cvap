box::use(
    magrittr[`%>%`],
    sh = shiny,
    e4r = echarts4r,
    dp = dplyr,
    pr = purrr,
    lub = lubridate,
    hw = htmlwidgets,
    fct = forcats,
    rl = rlang[`%||%`],
    gg = ggplot2,
    pal = palettes,
    mg = magick,
    cow = cowplot,
)

box::use(
    srqlib / srqcolor,
    app / logic / theme,
    aui = app / logic / aux_ui,
)

palette <- c(
    "#32575c",
    "#4f9e63",
    "#9fd685",
    "#c6d9bd"
)

#' @export
ui <- function(id) {
    ns <- sh$NS(id)
    aui$card(
        header = sh$div(
            class = "d-flex flex-row justify-content-between align-items-center",
            sh$div(
                class = "d-flex flex-row align-items-center",
                "Karta",
                aui$btn_modal(
                    ns("info-stapel"),
                    label = sh$icon("circle-info"),
                    modal_title = "Information om karta",
                    footer_confirm = NULL,
                    footer_dismiss = NULL,
                    class_toggle = "btn btn-transparent",
                    "Infotext om karta"
                )
            ),
            aui$inp_toggle(ns("load"), "Visa karta"),
        ),
        body = e4r$echarts4rOutput(ns("map")),
        footer = sh$div(
            class = "d-flex justify-content-start align-items-center gap-3",
            sh$downloadButton(ns("exmap"), "Download", class = "hover")
        )
    )
}

#' @export
server <- function(id, .data, geo, stash = NULL, x = "lan", y = "outcome", group = NULL, text = "Title") {
    sh$moduleServer(id, function(input, output, session) {
        stopifnot(sh$is.reactive(.data))

        time_vars <- c("visit_group", "timestamp")

        res_interactive <- sh$reactive({
            if (isFALSE(input$load)) { # abort if load is FALSE
                NULL
            } else { # otherwise draw map
                if (!is.null(group) && nrow(.data()) > 0 && !all(is.na(.data()[[y]]))) {
                    out <-
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


                    lvls <-
                        if (is.factor(out[[group]])) {
                            levels(out[[group]])
                        } else {
                            sort(unique(out[[group]]))
                        }


                    title <- pr$map(lvls, \(x) {
                        list(
                            text = paste0(text, ", ", x),
                            subtext = paste0("Data uttagen: ", lub$today()),
                            textStyle = list(color = "black", fontWeight = "bolder")
                        )
                    })
                } else {
                    out <- .data()

                    title <- list(
                        text = text,
                        subtext = paste0("Data uttagen: ", lub$today()),
                        textStyle = list(color = "black", fontWeight = "bolder")
                    )
                }

                # In visual_map, change...
                # Label legend limits: text = list("High", "Low")
                # Legend position: left = "right"

                basic <- out %>%
                    e4r$e_charts_(x, timeline = if (!is.null(group)) TRUE else FALSE) %>%
                    e4r$e_map_register("Sweden", geo$json) %>%
                    e4r$e_map_(y, map = "Sweden", nameProperty = "NAME_1") %>%
                    e4r$e_visual_map_(min = min(out[[y]]), max = max(out[[y]]), color = palette) %>%
                    e4r$e_theme("infographic") %>%
                    e4r$e_toolbox_feature(feature = c("saveAsImage"))

                if (!is.null(group)) {
                    basic %>%
                        e4r$e_timeline_opts(autoPlay = FALSE) %>%
                        e4r$e_timeline_serie(title = title)
                } else {
                    basic %>%
                        e4r$e_title(text = text, paste0("Data uttagen: ", lub$today()))
                }
            }
        })

        res_export <- sh$reactive({
            limits <- c(min(.data()[[y]], na.rm = TRUE), max(.data()[[y]], na.rm = TRUE))

            geo$sf %>%
                dp$left_join(.data(), dp$join_by("NAME_1" == "lan")) %>%
                gg$ggplot() +
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
        })

        output$map <- e4r$renderEcharts4r({
            sh$req(nrow(.data()) > 0 && !all(is.na(.data()[[y]])))
            res_interactive()
            })

        output$exmap <- sh$downloadHandler(
            filename = function() {
                paste0(lub$today(), "_vapX_map", ".pdf")
            },
            content = function(file) {
                gg$ggsave(file, res_export(), width = 5, height = 7)
            }
        )
    })
}
