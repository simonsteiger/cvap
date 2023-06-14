box::use(
    magrittr[`%>%`],
    fst,
    stats,
    sh = shiny,
    dp = dplyr,
    ts = tidyselect,
    rl = rlang,
    gg = ggplot2,
    ts = tidyselect,
    lub = lubridate,
    fct = forcats,
    str = stringr,
    pr = purrr,
)

box::use(
    ase = app / logic / aux_server,
    srqlib / srqprep,
)

#' @export
ui <- function(id, ...) {
    ns <- sh$NS(id)
    sh$tagList(
        ...
    )
}

#' @export
server <- function(id, .data, ...) {
    sh$moduleServer(id, function(input, output, session) {
        stopifnot(sh$is.reactive(.data))

        dots <- rl$quos(...)

        out <- sh$reactive(
            # If data is preprocessed, no need to floor_date
            if (str$str_detect(deparse(substitute(.data)), "^dat_.+")) {
                .data()
            } else {
                .data() %>%
                    dp$mutate(dp$across(ts$where(lub$is.Date), \(x) lub$floor_date(x, "years")))
            }
        )

        syn <- sh$reactive(
            out() %>%
                ase$synopsise(...)
        )
    })
}
