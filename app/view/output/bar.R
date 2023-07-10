box::use(
    magrittr[`%>%`],
    sh = shiny,
    e4r = echarts4r,
    dp = dplyr,
    lub = lubridate,
    fct = forcats,
    gg = ggplot2,
    ggt = ggtext,
)

box::use(
    ase = app / logic / aux_server,
    aui = app / logic / aux_ui,
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
            class = "d-flex justify-content-start",
            aui$btn_modal(
                ns("download"),
                label = sh$tagList(sh$icon("download"), "Download"),
                modal_title = "Anpassa download",
                footer_confirm = NULL,
                footer_dismiss = NULL,
                sh$plotOutput(ns("export"))
            )
        )
    )
}

#' @export
server <- function(id, .data, stash = NULL, x = "lan", y = "outcome", group = NULL, text = "Title", format = NULL, timeline = FALSE) {
    sh$moduleServer(id, function(input, output, session) {
        stopifnot(sh$is.reactive(.data))

        if (!is.null(group)) {
            out <- sh$reactive(
                .data() %>%
                    dp$mutate(
                        !!group := {
                            # Only sort by y if group does not imply chronological order
                            if (!lub$is.Date(.data[[group]]) && group != "visit_group") {
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
                e4r$e_legend(bottom = 0) %>%
                e4r$e_toolbox_feature(feature = c("saveAsImage")) %>%
                e4r$e_title(text, paste0("Data uttagen: ", lub$today()))

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
            gg$ggplot(out(), gg$aes(.data[[x]], .data[[y]])) +
                gg$geom_col(gg$aes(fill = as.factor(.data[[group]]))) +
                gg$coord_flip() +
                gg$ylab(NULL) +
                gg$xlab(NULL) +
                gg$labs(
                    title = stash()$title,
                    subtitle = stash()$subtitle
                    ) +
                gg$theme(
                    plot.subtitle = ggt$element_textbox_simple(
                        # family = "Commissioner",
                        # colour = dark_col,
                        hjust = 0,
                        lineheight = 0.5,
                        margin = gg$margin(t = 20, b = 10)
                    )
                )
        })

        output$bar <- e4r$renderEcharts4r(res_interactive())

        output$export <- sh$renderPlot(res_export())
    })
}
