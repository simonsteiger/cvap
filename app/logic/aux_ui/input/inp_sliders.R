box::use(
  sh = shiny
)

#' @export
#' Default wrapper of sliderInput with presets
inp_slider <- function(id, label, min, max, value) {
  stopifnot(length(value) == 2)
  stopifnot(is.numeric(c(min, max)))

  sh$div(
    class = "mb-4 mx-1",
    sh$sliderInput(
      inputId = id,
      label = label,
      min = min,
      max = max,
      value = value
    )
  )
}

#' @export
#' age sliderInput with preset label, min, max and range
inp_slider_age <- function(id) {
  inp_slider(
      id = id,
      label = "Välj åldersintervall",
      min = 0,
      max = 100,
      value = c(18, 100)
    )
}