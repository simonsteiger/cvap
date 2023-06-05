box::use(
    str = stringr,
    lub = lubridate,
)

#' @export
sift_cols <- function(col, val, var, skip) {
    if (var %in% skip || # dev-specified skip var
        is.null(val) || # input name unavailable (hidden)
        any(str_detect(val, "Alla?$|Båda")) || # user-specified skip
        var == "tidig_ra" && FALSE %in% val
    ) { # tidig_ra needed special treatment
        return(TRUE)
    } else if (is.logical(val)) { # logical before numeric to not treat logical as numeric
        return(as.logical(col) == val)
    } else if (is.numeric(col) && length(val) == 1) {
        return(!is.na(col) & col >= as.numeric(val))
    } else if (is.numeric(col) && length(val) == 2) { # numeric range
        return(!is.na(col) & col >= min(val) & col <= max(val))
    } else if (is.Date(col) && length(val) == 1) {
        return(!is.na(col) & col >= val)
    } else if (is.Date(col) && length(val) == 2) { # date range
        return(!is.na(col) & col >= min(val) & col <= max(val))
    } else if (is.factor(col)) {
        return(!is.na(col) & col %in% val)
    } else {
        # No control, so don't filter
        TRUE
    }
}
