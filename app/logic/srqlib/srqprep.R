box::use(
    cli,
    rl = rlang,
    lub = lubridate,
    dp = dplyr,
    pr = purrr,
)

#' @export
prep_ongoing <- function(.data,
                         .start,
                         .end,
                         .start_var,
                         .end_var,
                         .unit = "years",
                         .new_name = "timestamp") {
    .start_var <- rl$enquo(.start_var)
    .end_var <- rl$enquo(.end_var)
    unit <- switch(.unit,
        "years" = lub$years,
        cli$cli_abort("{unit} not supported. Currently supporting only 'year'.")
    )
    diff <- lub$interval(.start, .end) / unit(1)
    steps <- unit(0:diff)
    date_vector <- .start + steps

    aux_filter_ongoing <- function(.data, t, .start_var, .end_var) {
        if (!rl$as_name(.start_var) %in% colnames(.data)) {
            cli$cli_abort(
                "{rl$as_name(.start_var)} is not a column in the data; please respecify .start_var'."
            )
        }
        if (!rl$as_name(.end_var) %in% colnames(.data)) {
            cli$cli_abort("{rl$as_name(.end_var)} is not a column in the data; please respecify '.end_var'.")
        }
        if (!lub$is.Date(.data[[rl$as_name(.start_var)]])) {
            cli$cli_abort("{rl$as_name(.start_var)} must be of type {.cls Date}.")
        }
        if (!lub$is.Date(.data[[rl$as_name(.end_var)]])) {
            cli$cli_abort("{rl$as_name(.end_var)} must be of type {.cls Date}.")
        }
        .data %>%
            dp$filter(
                .data[[.start_var]] < t,
                is.na(.data[[.end_var]]) | .data[[.end_var]] > t
            ) %>%
            dp$mutate(
                !!.new_name := t
            )
    }

    pr$map(
        date_vector,
        ~ aux_filter_ongoing(.data = .data, t = .x, .start_var = .start_var, .end_var = .end_var)
    ) %>%
        pr$list_rbind()
}