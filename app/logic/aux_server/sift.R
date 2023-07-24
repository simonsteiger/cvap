box::use(
    sh = shiny,
    str = stringr,
    lub = lubridate,
    pr = purrr,
    dp = dplyr,
    rl = rlang[`%||%`],
    magrittr[`%>%`],
)

box::use(
    srqlib / srqprep,
    srqlib / srqdict,
)

#' @export
#' ATTENTION does not work with by-item-selection of numeric vars
#' Numeric vars are always treated as ranges or cut-offs
#' For by-item-selection, treat respective vars as factors or characters!
sift_cols <- function(col, val, var, skip) {
    if (var %in% skip || # dev-specified skip var
        is.null(val) || # input name unavailable (hidden)
        any(str$str_detect(val, "Alla?$|Båda")) || # user-specified skip
        var == "tidig_ra" && FALSE %in% val
    ) { # tidig_ra needed special treatment
        return(TRUE)
    } else if (is.logical(val)) { # logical before numeric to not treat logical as numeric
        return(as.logical(col) == val)
    } else if (is.numeric(col) && length(val) == 1) {
        return(!is.na(col) & col >= as.numeric(val))
    } else if (is.numeric(col) && length(val) == 2) { # numeric range
        return(!is.na(col) & col >= min(val) & col <= max(val))
    } else if (is.numeric(col) && length(val) == 2) { # numeric range
        return(!is.na(col) & col >= min(val) & col <= max(val))
    } else if (lub$is.Date(col) && length(val) == 1) {
        return(!is.na(col) & col <= val) # for ongoing
    } else if (lub$is.Date(col) && length(val) == 2) { # date range
        return(!is.na(col) & col >= min(val) & col <= max(val))
    } else if (is.factor(col)) {
        return(!is.na(col) & col %in% val)
    } else if (is.character(col)) {
        return(!is.na(col) & col %in% val)
    } else {
        # No control, so don't filter
        TRUE
    }
}

#' @export
sift_vars <- function(data, input, skip = NULL) {
    stopifnot(sh$is.reactive(data))

    # TODO add error if user selects no lans

    vars <- sh$reactive(colnames(data()))

    sh$reactive({
        each_var <-
            pr$map(vars(), \(v) sift_cols(data()[[v]], input[[v]], v, skip))

        pr$reduce(each_var, `&`)
    })
}

#' @export
#' Helper function to filter for ongoing cases if input$ongoing exists
#' and if we are not also setting lookback window (has its own ongoing filter)
maybe_ongoing <- function(.data, input) {
    if (is.null(input$ongoing) || !is.null(input$lookback)) {
        return(.data)
    } else {
        srqprep$prep_ongoing(
            .data,
            .start = min(input$ongoing),
            .end = max(input$ongoing),
            .start_var = ordinerat,
            .end_var = utsatt,
            .new_name = "ongoing_timestamp"
        )
    }
}

#' @export
#' Helper function to set lookback window if input$lookback exists
maybe_lookback <- function(.data, input, .var) {
    if (is.null(input$lookback)) {
        return(.data)
    } else {
        .data %>%
            # separately get rid of missing outcome values to avoid picking NA outcomes
            dp$filter(!is.na(.data[[input$outcome %||% .var]])) %>%
            dp$filter(
                !!srqdict$fil_ongoing(input$ongoing),
                datum >= input$ongoing - as.numeric(input$lookback) * lub$dyears(1)
            ) %>%
            dp$arrange(patientkod, dp$desc(datum)) %>%
            dp$distinct(patientkod, .keep_all = TRUE)
    }
}

