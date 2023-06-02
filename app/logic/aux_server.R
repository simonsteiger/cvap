box::use(
    sh = shiny,
    rt = shiny.router,
)

#' @export
#' Return to home page when return button is clicked
observe_return <- function(input) {
    sh$observeEvent(input$return, {
        rt$change_page("/")
    })
}