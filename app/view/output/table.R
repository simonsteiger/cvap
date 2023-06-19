box::use(
    magrittr[`%>%`],
    sh = shiny,
    dp = dplyr,
    rtbl = reactable,
)

#' @export
ui <- function(id) {
    ns <- sh$NS(id)
    sh$tagList()
}

#' @export
server <- function(id, .data, arrange = NULL) {
    sh$moduleServer(id, function(input, output, session) {
        stopifnot(sh$is.reactive(.data))

        sh$reactive(
            rtbl$reactable(
                .data() %>%
                    dp$arrange(dp$across(arrange))
            )
        )
    })
}
