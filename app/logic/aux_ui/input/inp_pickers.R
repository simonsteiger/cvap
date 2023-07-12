box::use(
    sh = shiny,
    sw = shinyWidgets,
)

#' @export
inp_picker <- function(id, label, choices, multiple = TRUE, sort = TRUE) {
    sh$div(
        class = "mb-4 mx-1",
        sw$pickerInput(
            inputId = id,
            label = label,
            choices = if (sort) sort(choices) else choices,
            selected = if (multiple) choices else NULL,
            multiple = multiple
        )
    )
}

#' @export
inp_picker_lan <- function(id, choices) {
    inp_picker(
        id,
        label = "Välj län",
        choices = choices,
    )
}

#' @export
inp_picker_dxcat <- function(id, choices) {
    inp_picker(
        id,
        label = "Välj diagnos",
        choices = choices,
        sort = FALSE
    )
}

#' @export
inp_picker_outcome <- function(id, choices) {
    inp_picker(
        id,
        label = "Välj utfallsmått",
        choices = choices,
        sort = FALSE
    )
}
