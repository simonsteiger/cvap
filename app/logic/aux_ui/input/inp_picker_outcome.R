box::use(
    sh = shiny,
    sw = shinyWidgets,
)

#' @export
inp_picker_outcome <- function(id) {
    sh$div(
        class = "mb-3 mx-1",
        sw$pickerInput(
            inputId = id,
            label = "Välj utfallsmått",
            choices = c("DAS28 < 3.2" = "das28_low", "CDAI <= 10" = "cdai_low"),
            multiple = FALSE
        )
    )
}
