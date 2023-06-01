box::use(
    sh = shiny,
    bsl = bslib,
    rt = shiny.router,
)

#' @export
return_button <- function(id) {
    sh$actionButton(id, label = "Return to menu", class = "hover")
}
