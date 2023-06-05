box::use(
    sh = shiny,
)

#' @export
logo <- function() {
    sh$div(
        class = "h3 d-flex justify-content-center",
        "SRQ Logo"
    )
}