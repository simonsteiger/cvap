box::use(
    magrittr[`%>%`],
    sh = shiny,
    e4r = echarts4r,
    dp = dplyr,
    lub = lubridate,
    fct = forcats,
)

box::use(
    ase = app / logic / aux_server,
)

#' @export
ui <- function(id) {
    ns <- sh$NS(id)
    sh$tagList()
}

#' @export
server <- function(id, .data, x = "lan", y = "outcome", group = NULL, text = "Title", format = NULL, timeline = FALSE) {
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

        sh$reactive({
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
    })
}
