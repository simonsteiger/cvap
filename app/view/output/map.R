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

worker <- ase$initialize_worker()

forward_args <- function(args) {
    ase$plot_map_interactive(
        .data = args$.data,
        geo = args$geo,
        x = args$x,
        y = args$y,
        group = args$group,
        title = args$title
    )
}

#' @export
ui <- function(id) {
    ns <- sh$NS(id)
    aui$card(
        header = sh$htmlOutput(ns("header")),
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
        data_formatted <- sh$reactive({
            if (format == "percent") {
                dp$mutate(.data(), !!y := round(.data[[y]] * 100, 0))
            } else {
                .data()
            }
        })

        # These variables imply chronological order and should be sorted differently
        time_vars <- c("visit_group", "timestamp") # used below in res_interactive

        # Create interactive map for in-app view
        args_map_interactive <- sh$reactive({
            # Assert that group is not null and there is some data to plot
            # indexing into .data would otherwise throw an error
            if (!is.null(group) && nrow(.data()) > 0 && !all(is.na(.data()[[y]]))) {
                out <-
                    data_formatted() %>%
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
                out <- data_formatted()

                title <- stash()$title
            }

            # No missings for interactive plot, but keep for export!
            data_interactive <- dp$filter(out, !is.na(outcome))

            # Return list with all arguments for ase$plot_map_interactive
            list(
                .data = data_interactive,
                geo = geo,
                x = x,
                y = y,
                group = group,
                title = title
            )
        })

        # Async job for map rendering
        promise_map <- worker$run_job(
            paste0("map_pid", paste0(sample(0:9, 4), collapse = "")),
            forward_args, # wraps plot_map_interactive
            args_map_interactive
        )

        # Render map when ready
        output$map <- e4r$renderEcharts4r({
            sh$req(nrow(.data()) > 0 && !all(is.na(.data()[[y]])))
            if (!is.null(promise_map()$result)) {
                promise_map()$result
            }
        })

        # Create header for map card which indicates when the map is loading
        output$header <- sh$renderUI({
            task <- promise_map()
            if (!task$resolved) {
                sh$tagList(
                    sh$div(
                        class = "d-flex justify-content-between align-items-center",
                        sh$div(
                            class = "py-card-header",
                            sh$tags$strong("Ritar karta, var god vänta...")
                        ),
                        sh$div(
                            class = "spinner-border spinner-border-sm",
                            role = "status"
                        )
                    )
                )
            } else {
                sh$div(
                    class = "d-flex flex-row align-items-center",
                    "Karta",
                    aui$btn_modal(
                        session$ns("info-karta"),
                        label = sh$icon("circle-info"),
                        modal_title = "Information om karta",
                        footer_confirm = NULL,
                        footer_dismiss = NULL,
                        class_toggle = "btn btn-transparent",
                        texts$info_map
                    )
                )
            }
        })

        # Create static map for download
        res_export <- sh$reactive({
            limits <- c(min(data_formatted()[[y]], na.rm = TRUE), max(data_formatted()[[y]], na.rm = TRUE))

            joined <- dp$left_join(geo$sf, data_formatted(), dp$join_by("NAME_1" == "lan"))

            ase$plot_map_export(joined, y, group, limits, stash)
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
