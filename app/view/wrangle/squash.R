box::use(
    magrittr[`%>%`],
    sh = shiny,
    dp = dplyr,
    rl = rlang,
    lub = lubridate,
    ts = tidyselect,
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
#' Summary module which reshapes the data frame
#' in other words, does a summary and not a mutate operation
server <- function(id, .data, .fn, .by, ...) {
    sh$moduleServer(id, function(input, output, session) {
        stopifnot(sh$is.reactive(.data))

        dots <- rl$list2(...)

        sh$reactive(
            .data() %>%
                dp$mutate(
                    dp$across(ts$where(lub$is.Date), \(x) lub$ceiling_date(x, "years") - 1)
                ) %>%
                dp$summarise(
                    outcome = .fn(!!!dots),
                    .by = ts$all_of(.by)
                )
        )
    })
}
