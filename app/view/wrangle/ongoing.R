box::use(
    magrittr[`%>%`],
    sh = shiny,
    dp = dplyr,
    pr = purrr,
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
server <- function(id, .data) {
    sh$moduleServer(id, function(input, output, session) {
        stopifnot(sh$is.reactive(.data))

        sh$reactive({
            srqprep$prep_ongoing(
                .data(),
                .start = min(input$ongoing),
                .end = max(input$ongoing),
                .start_var = ordinerat,
                .end_var = utsatt,
                .new_name = "ongoing_timestamp"
            )
        })
    })
}
