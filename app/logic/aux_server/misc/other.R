box::use(
  sh = shiny,
  rt = shiny.router,
  rl = rlang,
  waiter,
)

#' @export
#' Return to home page when return button is clicked
obs_return <- function(input) {
  sh$observeEvent(input$return, {
    rt$change_page("/")
  })
}

#' @export
#' Standardized html element for waiting screens
#' Provide html tags to customize content further
waiting_screen <- function(...) {
  dots <- rl$list2(...)
  sh$div(
    class = "fs-4 d-flex flex-column gap-3",
    waiter$spin_folding_cube(),
    !!!dots
  )
}
