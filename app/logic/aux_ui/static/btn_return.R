box::use(
    sh = shiny,
)

#' @export
btn_return <- function(id) {
    sh$actionButton(id, label = "Tillbaka", icon = sh$icon("angles-left"), class = "hover p-2")
}
