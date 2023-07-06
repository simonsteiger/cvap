box::use(
  sh = shiny
)

#' @export
inp_slider <- function(id, label, min, max, value) {
  stopifnot(length(value) == 2)
  stopifnot(is.numeric(c(min, max)))

  sh$div(
    class = "mb-3 mx-1",
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
inp_slider_age <- function(id) {
  inp_slider(
      id = id,
      label = "Välj åldersintervall",
      min = 0,
      max = 100,
      value = c(18, 100)
    )
}