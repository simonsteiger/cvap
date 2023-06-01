box::use(
    sh = shiny,
    rt = shiny.router,
)

#' @export
observe_return <- function(input) {
    sh$observeEvent(input$return, {
        rt$change_page("/")
    })
}