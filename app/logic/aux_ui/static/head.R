box::use(
    sh = shiny,
)

#' @export
head <- function(...) {
    sh$tagList(
        sh$div(class = "h1 text-center", "Visualiserings- och analysplattform"),
        ...
    )
}
