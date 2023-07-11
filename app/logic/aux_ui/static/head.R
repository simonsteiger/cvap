box::use(
    sh = shiny,
)

#' @export
head <- function(text = "Visualiserings- och analysplattform", ...) {
    sh$tagList(
        sh$div(class = "fs-1 h-font text-center", text),
        ...
    )
}
