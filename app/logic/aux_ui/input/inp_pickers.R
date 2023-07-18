box::use(
    sh = shiny,
    sw = shinyWidgets,
    rl = rlang[`%||%`],
)

#' @export
inp_picker <- function(id, label, choices, multiple = TRUE, sort = TRUE, options = list()) {
    sh$div(
        class = "mb-4 mx-1",
        sw$pickerInput(
            inputId = id,
            label = label,
            choices = if (sort) sort(choices) else choices,
            selected = if (multiple) choices else NULL,
            multiple = multiple,
            options = options,
        )
    )
}

#' @export
inp_picker_lan <- function(id, choices) {
    inp_picker(
        id,
        label = "Välj län",
        choices = choices,
        options = list(`actions-box` = TRUE)
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

#' @export
inp_picker_timestamp <- function(id, choices, unit = NULL) {
    inp_picker(
        id,
        label = paste("Välj jämforelser", unit),
        choices = choices
    )
}
