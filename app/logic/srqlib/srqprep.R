box::use(
    cli,
    rl = rlang,
    lub = lubridate,
    dp = dplyr,
    pr = purrr,
    fct = forcats,
    tdr = tidyr,
    ts = tidyselect,
    magrittr[`%>%`],
)

box::use(
    app / logic / swissknife / sklang[`%//%`],
)

# Helper for filter_ongoing
aux_filter_ongoing <- function(.data, t, .start_var, .end_var, .new_name) {
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

    pr$map(
        date_vector,
        ~ aux_filter_ongoing(.data, .x, .start_var, .end_var, .new_name)
    ) %>%
        pr$list_rbind()
}

#' @export
prep_custom_order <- function(.data, .reorder, .by, ...) {
    reorder <- rl$enquo(.reorder)
    by <- rl$enquo(.by)
    dots <- rl$quos(...)

    if (!is.numeric(.data[[.by]])) {
        cli$cli_abort("{.var .by} must be {.cls numeric}")
    }

    if (rl$quo_is_missing(dots[[1]])) {
        cli$cli_abort("Use {.var .reorder = .by} in autoplot instead")
    }

    .data %>%
        dp$mutate(
            sort = pr$map_dbl(
                .data[[.reorder]],
                ~ .data[[.by]][!!!dots & .data[[.reorder]] == .x] %//% 0
            ),
            !!.reorder := as.factor(.data[[.reorder]]),
            y_na_zero = ifelse(is.na(sort), 0, sort),
            !!.reorder := fct$fct_reorder(.data[[.reorder]], y_na_zero, .na_rm = FALSE)
        ) %>%
        dp$arrange(dp$across(.data[[.reorder]])) %>%
        dp$select(-c(sort, y_na_zero))
}

#' @export
prep_recode <- function(.data, .var, dict, .default = NA, .new_name = NULL) {
    .var <- rl$enquo(.var)
    .new_name <- rl$enquo(.new_name)
    if (is.null(rl$quo_get_expr(.new_name))) .new_name <- .var

    if (is.list(dict)) {
        if (is.null(dict$.default) || !is.na(.default)) {
            dict$.default <- .default
        }

        out <- .data %>%
            dp$mutate(
                !!.new_name := dp$case_when(!!!dict)
            )
    } else if (is.character(dict)) {
        out <- .data %>%
            dp$mutate(
                !!.new_name := as.factor(.data[[.var]]),
                !!.new_name := fct$fct_recode(.data[[.var]], !!!dict)
            )
    } else {
        cli$cli_abort("{.emph dict} must be a named {.cls list} or a named {.cls character} vector.")
    }
}

# Helper for prep_dynamic_groups
tidy_helper <- function(x) {
    if (is.character(rl$quo_get_expr(x))) {
        rl$quo_get_expr(x)
    } else if (is.symbol(rl$quo_get_expr(x))) {
        x
    } else {
        cli$cli_abort(c(
            "{x {x}} must be a {.cls character} or {.cls symbol}.",
            "x" = "But it is type {.cls {class(x)}}"
        ))
    }
}

#' @export
prep_dynamic_groups <- function(.data, .start, .end, .start_var, .end_var, ..., .new_name = "visit_group") {
    # Different version than the one in srqlib repo - this one is adated for use in functions
    # Have to reconcile both eventually
    .start_var <- if (is.character(.start_var)) .start_var else rl$enquo(.start_var)
    .end_var <- if (is.character(.start_var)) .start_var else rl$enquo(.start_var)

    dots <- rl$quos(...)

    if (rl$as_name(.start_var) == "first_visit") {
        .data <- .data %>%
            dp$mutate(
                first_visit = min(datum),
                last_visit = max(datum),
                .by = patientkod
            )
    }

    # the plots checking for inclusion after symptom debut look into the past
    # filtering by .start_var below leads to inconsistencies
    # should be improved when all plots done
    .data <- .data %>%
        dp$filter(
            .data[[.start_var]] >= .start,
            .data[[.start_var]] < .end
        ) %>%
        dp$mutate(
            diff = lub$interval(.data[[.start_var]], .data[[.end_var]]) / lub$ddays(1),
            !!.new_name := dp$case_when(!!!dots)
        ) %>%
        tdr$drop_na(ts$any_of(.new_name))
}

#' @export
prep_line <- function(.data, id_col = "id") {
    req_cols <- unique(c(id_col, "patientkod", "ordinerat", "utsatt"))
    found_cols <- req_cols %in% colnames(.data)
    if (!all(found_cols)) {
        cli$cli_abort(c(
            "Expecting columns named {.var {req_cols}}.",
            "x" = "Missing column{?s} {.var {req_cols[!found_cols]}}."
        ))
    }
    .data %>%
        dp$arrange(patientkod, ordinerat, dp$desc(utsatt)) %>%
        dp$mutate(
            line = seq_along(.data[[id_col]]),
            .by = c(patientkod)
        )
}

#' @export
prep_riket <- function(.data, .var, .fn, .by, ...) {
    var <- rl$enquo(.var)
    by <- rl$enquo(.by)
    dots <- rl$list2(...)

    # I run into an LHS error if .var is unquoted below
    # => it becomes a call after enquo
    # check if var is a call, if so, make it a character again
    if (is.call(var)) var <- .var

    riket <- .data %>%
        dp$summarise(
            lan = "Riket",
            !!.var := .fn(.data[[var]], !!!dots),
            .by = ts$all_of(.by)
        )

    dp$bind_rows(.data, riket)
}
