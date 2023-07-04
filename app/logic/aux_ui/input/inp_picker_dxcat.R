box::use(
    sh = shiny,
    sw = shinyWidgets,
)

#' @export
inp_picker_dxcat <- function(id, choices, multiple = FALSE) {
    sh$div(
        class = "mb-3 mx-1",
        sw$pickerInput(
            inputId = id,
            label = "Välj diagnos",
            choices = sort(choices),
            selected = sort(choices),
            multiple = multiple
        )
    )
}
