box::use(
    sh = shiny,
)

#' @export
inp_toggle_sort <- function(id, class = NULL) {
    if (!is.null(class)) {
        class <- paste("form-check form-switch", class, " ")
    } else {
        class <- "form-check form-switch"
    }

    sh$div(
        class = class,
        sh$tags$input(class = "form-check-input", type = "checkbox", role = "switch", id = id),
        sh$tags$label(class = "form-check-label", `for` = id, "Alfabetisk ordning")
    )
}
