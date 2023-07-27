box::use(
    pr = purrr,
    lub = lubridate,
    magrittr[`%>%`],
    dp = dplyr,
)

box::use(
    swissknife / sklang[`%//%`, `%na?%`],
)

#' @export
#' Validate if the date input is a valid date
vali_date <- function(input) {
    # These are the candidates for date inputs
    options <- c("inkluderad", "ordinerat", "ongoing")

    var <- `%//%`( # Check if length of first arg is 0, if TRUE, return second arg
        options[options %in% names(input)],
        return(list(valid = TRUE, inrange = TRUE, var = NULL))
    )

    # Check if each entry of input[[var]] is of type Date
    check1 <- all(pr$map_lgl(input[[var]], lub$is.Date))

    # If check1 is TRUE and input[[var]] contains two dates,
    # check if they are both within 1999-01-01 and today's date
    check2 <- if (check1 && length(input[[var]]) == 2) {
        prelim <- min(input[[var]]) >= lub$ymd("1999-01-01") & max(input[[var]]) <= lub$today()
        # ymd seems to convert some strange dates into NA (not sure why they pass check1)
        # If this happens, prelim will be NA, and the %na?% infix will return FALSE
        prelim %na?% FALSE
    } else if (check1 && length(input[[var]]) == 1) {
        prelim <- input[[var]] >= lub$ymd("1999-01-01") & input[[var]] <= lub$today()
        prelim %na?% FALSE # See above condition
    } else {
        FALSE
        # If other conditions were FALSE, return FALSE
    }

    # Return results of check1, check2 and variable name `var`
    list(valid = check1, inrange = check2, var = var)
}

#' @export
check_samplesize <- function(.data) {
    lans_above_cutoff <- .data %>%
        dp$summarise(n = dp$n(), .by = lan) %>%
        dp$filter(n > 4) %>% # == <5
        nrow()

    if (lans_above_cutoff > 0) TRUE else FALSE
}
