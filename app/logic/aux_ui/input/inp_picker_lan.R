box::use(
    sw = shinyWidgets,
)

inp_picker_lan <- function(id, choices) {
    sw$pickerInput(
        inputId = id,
        label = "Välj län",
        choices = sort(choices),
        selected = sort(choices),
        multiple = TRUE
    )
}
