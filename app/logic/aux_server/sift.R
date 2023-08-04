box::use(
    sh = shiny,
    str = stringr,
    lub = lubridate,
    pr = purrr,
    dp = dplyr,
    shf = shinyFeedback,
    rl = rlang[`%||%`],
    magrittr[`%>%`],
)

box::use(
    app / logic / aux_server / checks,
    app / logic / srqlib / srqprep,
    app / logic / srqlib / srqdict,
)

#' @export
#' REMINDER sift_cols() does not work with by-item-selection of numeric vars
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

# For testing purposes only, currently convoluted solution as I don't
# understand how to test server$sift
# For the tests to be correct, apply any changes you make to the "real" sift_vars here too
test_sift_vars <- function(data, input, skip = NULL) {
    vars <- colnames(data)
    each_var <- pr$map(vars, \(v) sift_cols(data[[v]], input[[v]], v, skip))
    pr$reduce(each_var, `&`)
}

#' @export
#' Loops over all column names in data which have matched inputs
#' Creates bool vectors for each as per sift_cols
#' And combines them into a single one with iterative `&` concatenation
sift_vars <- function(data, input, skip = NULL, test = FALSE) {
    if (test) {
        return(test_sift_vars(data, input, skip))
    }

    stopifnot(sh$is.reactive(data))

    vars <- sh$reactive(colnames(data()))

    sh$reactive({
        each_var <-
            pr$map(vars(), \(v) sift_cols(data()[[v]], input[[v]], v, skip))

        # Apply `&` iteratively to pairs of bool vectors in each_var
        # until there is only one bool vector left
        # This vector will extract those rows where all filter conditions where TRUE
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

#' @export
#' Calculate sample size while excluding samples from lans with < 5 ppl
#' This is prone to inaccuracy where lan data is further grouped in synopsis
#' e.g. into Behandlingsstart and Uppföljning
#' => total n per lan > 5, but < 5 in subgroups
count_nonmissing_above_cutoff <- function(.data, input, .var) {
    if (!is.null(input$lan) && nrow(.data) > 0) {
        .data %>%
            dp$summarise(
                nonmissing = sum(!is.na(.data[[.var %||% input$outcome]])),
                .by = lan
            ) %>%
            dp$filter(nonmissing > 5) %>%
            dp$pull(nonmissing) %>%
            sum()
    } else {
        0
    }
}

#' @export
#' Counts remaining cases after filtering and displays result as a pseudo-button
#' Also warns if invalid date selected and / or no lans selected
sift_feedback <- function(.data, input, .var, button) {
    stopifnot(!sh$is.reactive(.data)) # non reactive required

    check_date <- checks$vali_date(input)

    # Warn if dates invalid
    shf$feedbackDanger(
        check_date$var,
        !check_date$inrange,
        "Välj två olika datum mellan 1999 och idag.",
        icon = NULL
    )

    # Warn if no lan selected
    shf$feedbackDanger(
        "lan",
        is.null(input$lan),
        "Välj minst ett län.",
        session = session
    )

    # Warn if no dxcat selected, currently does not prevent `go`
    shf$feedbackDanger(
        "dxcat",
        is.null(input$dxcat),
        "Välj minst en diagnos.",
        session = session
    )

    n <- count_nonmissing_above_cutoff(.data, input, .var)

    if (!button) {
        return(NULL)
    }

    sh$tags$button(
        type = "button",
        style = "pointer-events: none;",
        class = ifelse(n > 0, "btn btn-secondary", "btn btn-danger"),
        sh$div(
            class = "d-flex flex-row align-items-center gap-2",
            if (n == 0) sh$icon("users-slash") else sh$icon("users"),
            if (n == 0) "Ingen data, anpassa urval" else paste0("Antal observationer: ", n)
        )
    )
}
