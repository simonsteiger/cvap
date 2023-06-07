box::use(
    magrittr[`%>%`],
    fst,
    stats,
    sh = shiny,
    dp = dplyr,
    gg = ggplot2,
    ts = tidyselect,
    lub = lubridate,
)

box::use(
    ase = app / logic / aux_server,
)

#' @export
ui <- function(id, ...) {
    ns <- sh$NS(id)
    sh$tagList(
        ...
    )
}

#' @export
server <- function(id, data, ...) { # pass the data of the current vap
    sh$moduleServer(id, function(input, output, session) {
        stopifnot(sh$is.reactive(data))

        sh$reactive(
            data() %>%
            dp$mutate(dp$across(ts$where(lub$is.Date), \(x) lub$floor_date(x, "years"))) %>%
            ase$synopsise(
                ...
            )
        )
    })
}
