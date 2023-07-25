box::use(
    pr = purrr,
    lub = lubridate,
)

box::use(
    swissknife / sklang[`%//%`],
)

#' @export
vali_date <- function(input) {
    options <- c("inkluderad", "ordinerat", "ongoing")

    var <- `%//%`( # Check if length of first arg is 0, if TRUE, return second arg
        options[options %in% names(input)],
        return(list(valid = TRUE, inrange = TRUE, var = NULL))
    )

    check1 <- all(pr$map_lgl(input[[var]], lub$is.Date))

    check2 <- if (check1) {
        min(input[[var]]) >= lub$ymd("1999-01-01") & max(input[[var]]) <= lub$today()
    } else {
        FALSE
    }

    list(valid = check1, inrange = check2, var = var)
}
