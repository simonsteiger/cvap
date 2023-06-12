box::use(
    sh = shiny,
)

#' @export
head <- function(...) {
    sh$tagList(
        sh$div(class = "fs-1 text-center", "Visualiserings- och analysplattform"),
        ...
    )
}
