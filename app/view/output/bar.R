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
                sh$htmlOutput(ns("inp_malniva")),
                aui$inp_toggle(ns("sort"), "Alfabetisk ordn.")
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
server <- function(id, .data, stash, text, x = "lan", y = "outcome", group = NULL, format = "decimal", timeline = FALSE, arrange = "lan") {
    sh$moduleServer(id, function(input, output, session) {
        stopifnot(sh$is.reactive(.data))

        output$inp_malniva <- sh$renderUI({
            if ("Riket" %in% .data()$lan) {
                aui$inp_toggle(session$ns("malniva"), "Visa mål")
            } else {
                NULL
            }
        })

        outcome_long <- sh$reactive(ase$translate_outcome(stash()$outcome))

        # These variables imply chronological order and should be sorted differently
        time_vars <- c("visit_group", "timestamp")

        out <- sh$reactive({
            # Assert that group isn't NULL and there is data to work with
            if (!is.null(group) && nrow(.data()) > 0 && !all(is.na(.data()[[y]]))) {
                dat_reorder <- .data() %>%
                    dp$mutate(
                        !!group := {
                            # Only sort by y if group does not imply chronological order
                            if (!lub$is.Date(.data[[group]]) && !group %in% time_vars) {
                                as.factor(.data[[group]]) %>% fct$fct_reorder(-.data[[y]])
                            } else {
                                format(.data[[group]], format = "%Y")
                            }
                        }
                    ) %>%
                    dp$group_by(.data[[group]]) %>%
                    dp$arrange(dp$across(arrange))
            } else {
                dat_reorder <- dp$arrange(.data(), dp$across(arrange))
            }
            dp$rename(dat_reorder, !!outcome_long() := outcome)
        })

        # Create echarts barplot
        res_interactive <- sh$reactive({
            out_basic <- ase$plot_bar_interactive(
                out(),
                input,
                x,
                outcome_long(),
                timeline,
                text,
                format
            )
        })

        # Create ggplot barplot for download as pdf (echarts offers only poor resolution)
        res_export <- sh$reactive({
            ase$plot_bar_export(out(), x, outcome_long(), group, timeline, stash(), input, format)
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
