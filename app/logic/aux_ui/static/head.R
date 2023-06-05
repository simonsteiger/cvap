box::use(
    sh = shiny,
)

#' @export
head <- function() {
    sh$div(class = "h1 text-center", "Visualiserings- och analysplattform")
}