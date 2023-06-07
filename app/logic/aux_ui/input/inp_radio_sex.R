box::use(
  sh = shiny,
  sw = shinyWidgets,
)

#' @export
inp_radio_sex <- function(id) {
  sh$div(
    class = "mb-3 mx-1",
    sw$prettyRadioButtons(
      inputId = id,
      label = "Välj kön",
      status = "primary",
      shape = "round",
      animation = "smooth",
      plain = TRUE,
      inline = TRUE,
      choices = c("Kvinna", "Man", "Båda"),
      selected = "Båda"
    )
  )
}
