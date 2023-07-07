box::use(
    magrittr[`%>%`],
    fst,
    stats,
    sh = shiny,
    dp = dplyr,
    ts = tidyselect,
    rl = rlang[`%||%`],
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
server <- function(id, .data, .fn, .var = "outcome", .by, riket = TRUE, ...) {
    sh$moduleServer(id, function(input, output, session) {
        stopifnot(sh$is.reactive(.data))

        dots <- rl$quos(...)

        outcome <- sh$reactive({
            input$outcome %||% .var
        })

        out <- sh$reactive({
            # If data is preprocessed, no need to floor_date
            if (str$str_detect(deparse(substitute(.data)), "^dat_.+")) {
                .data()
            } else {
                .data() %>%
                    dp$mutate(dp$across(ts$where(lub$is.Date), \(x) lub$floor_date(x, "years")))
            }
        })

        dat_riket <- sh$reactive({
            if (riket) {
                srqprep$prep_riket(out(), outcome(), .fn, .by, !!!dots)
            } else {
                out()
            }
        })

        dat_sum <- sh$reactive({
            dat_riket() %>%
                dp$summarise(
                    outcome = .fn(.data[[outcome()]], !!!dots),
                    missing = sum(is.na(.data[[outcome()]])),
                    nonmissing = sum(!is.na(.data[[outcome()]])),
                    .by = ts$all_of(.by)
                )
        })

        digits <- sh$reactive({
            dp$case_when(
                max(dat_sum()[["outcome"]]) <= 10 ~ 2,
                max(dat_sum()[["outcome"]]) %>% dp$between(10.01, 99.99) ~ 1,
                max(dat_sum()[["outcome"]]) >= 100 ~ 0,
            )
        })

        sh$reactive({
            dat_sum() %>%
                dp$mutate(
                    outcome = round(.data[["outcome"]], digits())
                )
        })
    })
}
