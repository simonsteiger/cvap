#' @export
#' Check if LHS is type(0); if TRUE return RHS, otherwise LHS
`%//%` <- function(x, y) {
    if (length(x) == 0 && !is.null(x)) y else x
}

#' @export
#' Check if LHS is NA; if TRUE return RHS, otherwise LHS
`%na?%` <- function(x, y) {
    if (is.na(x)) y else x
}

#' @export
#' Check if LHS is NA; if TRUE return RHS, otherwise LHS
`%nan?%` <- function(x, y) {
    if (is.nan(x)) y else x
}