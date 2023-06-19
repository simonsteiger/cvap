box::use(
    magrittr[`%>%`],
    sh = shiny,
    dp = dplyr,
    pr = purrr,
    lub = lubridate,
    rl = rlang[`%||%`],
)

box::use(
    srqlib / srqdict,
    srqlib / srqprep,
)

#' @export
ui <- function(id) {
    ns <- sh$NS(id)
    sh$tagList()
}

#' @export
server <- function(id, .data, .var) {
    sh$moduleServer(id, function(input, output, session) {
        stopifnot(sh$is.reactive(.data))

        sh$reactive({
            .data() %>%
                dp$rename(outcome = .data[[.var]]) %>%
                # separately get rid of missing outcome values to avoid picking NA outcomes
                dp$filter(!is.na(outcome)) %>%
                dp$filter(
                    !!srqdict$fil_ongoing(input$ongoing),
                    datum >= input$ongoing - as.numeric(input$lookback) * lub$dyears(1)
                ) %>%
                dp$arrange(patientkod, dp$desc(datum)) %>%
                dp$distinct(patientkod, .keep_all = TRUE)
        })
    })
}
