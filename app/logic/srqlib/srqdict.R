box::use(
    rl = rlang,
)

#' @export
fil_ongoing <- function(at) {
    rl$quo(
        ordinerat <= at & (pagaende == 1 | utsatt > at)
    )
}