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
)

box::use(
    srqlib / srqcolor,
)

palette <- c(
    "#32575c",
    "#4f9e63",
    "#9fd685",
    "#c6d9bd"
)

#' @export
plot_map <- function(.data, geo, x = "lan", y = "outcome", group = NULL, text = "Title") {
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

        lvls <- sh$reactive(unique(out()[[group]]))

        title <- sh$reactive(pr$map(lvls(), \(x) {
            list(
                text = paste0(text, ", ", x),
                subtext = paste0("Data uttagen: ", lub$today()),
                textStyle = list(color = "black", fontWeight = "bolder")
            )
        }))
    } else {
        out <- .data

        title <- list(
            text = text,
            subtext = paste0("Data uttagen: ", lub$today()),
            textStyle = list(color = "black", fontWeight = "bolder")
        )
    }

    sh$reactive({
        basic <- out() %>%
            e4r$e_charts_(x, timeline = if (!is.null(group)) TRUE else FALSE) %>%
            e4r$e_map_register("Sweden", geo) %>%
            e4r$e_map_(y, map = "Sweden", nameProperty = "NAME_1") %>%
            e4r$e_visual_map_(y, color = palette) %>%
            e4r$e_theme("infographic") %>%
            e4r$e_toolbox_feature(feature = c("saveAsImage"))

        if (!is.null(group)) {
            basic %>%
                e4r$e_timeline_opts(autoPlay = FALSE) %>%
                e4r$e_timeline_serie(title = title())
        } else {
            basic %>%
                e4r$e_title(text = text, paste0("Data uttagen: ", lub$today()))
        }
    })
}
