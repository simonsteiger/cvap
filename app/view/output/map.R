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
    str = stringr,
)

box::use(
    app / logic / srqlib / srqcolor,
    app / logic / theme,
    aui = app / logic / aux_ui,
    ase = app / logic / aux_server,
    app / logic / data / texts,
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
                    ns("info-karta"),
                    label = sh$icon("circle-info"),
                    modal_title = "Information om karta",
                    footer_confirm = NULL,
                    footer_dismiss = NULL,
                    class_toggle = "btn btn-transparent",
                    texts$info_map
                )
            )
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

        # Convert outcome to percent if formatter is specified to "percent"
        # Always remove missings here
        formatted_data <- sh$reactive({
            if (format == "percent") {
                dp$mutate(.data(), !!y := round(.data[[y]] * 100, 0))
            } else {
                .data()
            }
        })

        # These variables imply chronological order and should be sorted differently
        time_vars <- c("visit_group", "timestamp") # used below in res_interactive

        # Create interactive map for in-app view
        res_interactive <- sh$reactive({
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
                                format(.data[[group]], format = "%Y")
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
                        text = stash()$title,
                        subtext = paste0("Data uttagen: ", lub$today()),
                        textStyle = list(fontFamily = "Commissioner"),
                        subtextStyle = list(fontFamily = "Roboto")
                    )
                })
            } else {
                out <- formatted_data()

                title <- stash()$title
            }
            # Assemble echart
            out %>%
                dp$filter(!is.na(outcome)) %>% # No missings for interactive plot, but keep for export!
                ase$plot_map_interactive(geo, x, y, group, title)
        })

        # Create static map for download
        res_export <- sh$reactive({
            limits <- c(min(formatted_data()[[y]], na.rm = TRUE), max(formatted_data()[[y]], na.rm = TRUE))

            joined <- dp$left_join(geo$sf, formatted_data(), dp$join_by("NAME_1" == "lan"))

            ase$plot_map_export(joined, y, group, limits, stash)
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
                active_vap <- str$str_extract(session$ns(id), "vap_\\w+_\\d")
                paste(lub$today(), active_vap, "karta.pdf", sep = "_")
            },
            content = function(file) {
                width <- if (!is.null(group)) 10 else 5
                height <- if (!is.null(group) && length(unique(.data()[[group]])) > 3) 9 else 7
                gg$ggsave(file, res_export(), width = width, height = height)
            }
        )
    })
}
