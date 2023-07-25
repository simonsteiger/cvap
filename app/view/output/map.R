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
    shj = shinyjs,
)

box::use(
    srqlib / srqcolor,
    app / logic / theme,
    aui = app / logic / aux_ui,
    ase = app / logic / aux_server,
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
            class = "d-flex justify-content-between align-items-center gap-3",
            sh$downloadButton(ns("exmap"), "Download", class = "hover"),
            sh$htmlOutput(ns("notification"))
        )
    )
}

#' @export
server <- function(id, .data, geo, stash = NULL, x = "lan", y = "outcome", group = NULL, text = "Title", format = "decimal") {
    sh$moduleServer(id, function(input, output, session) {
        stopifnot(sh$is.reactive(.data))

        # Notification to show when map is loading
        output$notification <- sh$renderUI(
            sh$div(
                class = "d-flex flex-row align-items-center gap-2",
                sh$div(class = "spinner-grow spinner-grow-sm c-warning", role = "status"),
                "Var god vänta...",
            )
        )

        # Default status is hidden
        shj$hide("notification")

        # Show notification when load switch is turned on
        sh$observe({
            if (isTRUE(input$load)) {
                shj$show("notification")
                shj$delay(2500, shj$hide("notification"))
            }
        })

        # Convert outcome to percent if formatter is specified to "percent"
        formatted_data <- sh$reactive({
            if (format == "percent") {
                dp$mutate(.data(), !!y := round(.data[[y]] * 100, 0))
            } else {
                .data()
            }
        })

        # Create interactive map for in-app view
        time_vars <- c("visit_group", "timestamp")

        res_interactive <- sh$reactive({
            if (isFALSE(input$load)) { # abort if load is FALSE
                NULL
            } else { # else draw map
                # Assert that group is not null and there is some data to plot
                # indexing into .data would otherwise throw an error
                if (!is.null(group) && nrow(.data()) > 0 && !all(is.na(.data()[[y]]))) {
                    out <-
                        formatted_data() %>%
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

                    # Create the title for element on the timeline
                    title <- pr$map(lvls, \(x) {
                        list(
                            text = paste0(text, ", ", x),
                            subtext = paste0("Data uttagen: ", lub$today()),
                            textStyle = list(fontFamily = "Commissioner"),
                            subtextStyle = list(fontFamily = "Roboto")
                        )
                    })
                } else {
                    out <- formatted_data()

                    title <- list(
                        text = text,
                        subtext = paste0("Data uttagen: ", lub$today()),
                        textStyle = list(fontFamily = "Commissioner"),
                        subtextStyle = list(fontFamily = "Roboto")
                    )
                }

                # Main assembly of echart
                basic <- out %>%
                    e4r$e_charts_(x, timeline = if (!is.null(group)) TRUE else FALSE) %>%
                    e4r$e_map_register("Sweden", geo$json) %>%
                    e4r$e_map_(y, map = "Sweden", nameProperty = "NAME_1") %>%
                    #e4r$e_tooltip(
                    #    formatter = ase$format_list[[format]](),
                    #    textStyle = list(fontFamily = "Roboto")
                    #) %>%
                    e4r$e_visual_map_(
                        min = min(out[[y]]),
                        max = max(out[[y]]),
                        color = palette,
                        textStyle = list(fontFamily = "Roboto")
                    ) %>%
                    e4r$e_theme_custom("app/static/echarts_theme.json")

                if (!is.null(group)) {
                    basic %>%
                        e4r$e_timeline_opts(autoPlay = FALSE) %>%
                        e4r$e_timeline_serie(title = title)
                } else {
                    basic %>%
                        e4r$e_title(
                            text = text,
                            paste0("Data uttagen: ", lub$today()),
                            textStyle = list(fontFamily = "Commissioner"),
                            subtextStyle = list(fontFamily = "Roboto")
                        )
                }
            }
        })

        # Create static map for download
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

        # Deactivate download button if no data
        sh$observe({
            cnd <- nrow(.data()) > 0 && !all(is.na(.data()[[y]]))
            shj$toggleState("exmap", cnd)
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
