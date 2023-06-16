box::use(
    magrittr[`%>%`],
    sh = shiny,
    dp = dplyr,
)

box::use(
    srqlib / srqdict,
)

#' @export
ui <- function(id) {
    ns <- sh$NS(id)
    sh$tagList()
}

#' @export
server <- function(id, .data) {
    sh$moduleServer(id, function(input, output, session) {
        stopifnot(sh$is.reactive(.data))

        sh$reactive({
            .data() %>%
                dp$filter(!!srqdict$fil_ongoing(input$ongoing))
        })
    })
}
