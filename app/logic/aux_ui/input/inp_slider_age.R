box::use(
  sh = shiny
)

#' @export
inp_slider_age <- function(id) {
  sh$div(
    class = "mb-4",
    sh$sliderInput(
      inputId = id,
      label = "Välj åldersintervall",
      min = 0,
      max = 100,
      value = c(18, 100)
    )
  )
}
