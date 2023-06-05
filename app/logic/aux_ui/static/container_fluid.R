box::use(
    sh = shiny,
)

#' @export
container_fluid <- function(...) {
    sh$div(
        class = "container-fluid m-5",
        ...
    )
}
