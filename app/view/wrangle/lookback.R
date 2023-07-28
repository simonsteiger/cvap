box::use(
    magrittr[`%>%`],
    sh = shiny,
    dp = dplyr,
    pr = purrr,
    lub = lubridate,
    rl = rlang[`%||%`],
)

box::use(
    app / logic / srqlib / srqdict,
    app / logic / srqlib / srqprep,
)

#' @export
lookback <- function(.data, input, .var = NULL) {
    stopifnot(!sh$is.reactive(.data)) # no reactive input
    .data %>%
        dp$rename(outcome = .data[[.var %||% input$outcome]]) %>%
        # separately get rid of missing outcome values to avoid picking NA outcomes
        dp$filter(!is.na(outcome)) %>%
        dp$filter(
            !!srqdict$fil_ongoing(input$ongoing),
            datum >= input$ongoing - as.numeric(input$lookback) * lub$dyears(1)
        ) %>%
        dp$arrange(patientkod, dp$desc(datum)) %>%
        dp$distinct(patientkod, .keep_all = TRUE)
}

#' @export
server <- function(id, .data, .var = NULL) {
    sh$moduleServer(id, function(input, output, session) {
        stopifnot(sh$is.reactive(.data))

        sh$reactive({
            lookback(.data(), input, .var)
        })
    })
}
