box::use(
    sh = shiny,
    sw = shinyWidgets,
    rl = rlang[`%||%`],
)

#' @export
#' Default wrapper of shinyWidgets' pickerInput with presets
inp_picker <- function(id, label, choices, selected = NULL, multiple = TRUE, sort = TRUE, options = list()) {
    sh$div(
        class = "mb-4 mx-1",
        sw$pickerInput(
            inputId = id,
            label = label,
            choices = if (sort) sort(choices) else choices,
            selected = if (!is.null(selected)) selected else if (multiple) choices else NULL,
            multiple = multiple,
            options = options,
        )
    )
}

#' @export
#' Preset picker for läns
inp_picker_lan <- function(id, choices, selected = NULL) {
    inp_picker(
        id,
        label = "Välj län",
        choices = choices,
        options = list(`actions-box` = TRUE),
        selected = selected
    )
}

#' @export
#' Preset picker for dxcat
inp_picker_dxcat <- function(id, choices, selected = NULL, multiple = TRUE) {
    inp_picker(
        id,
        label = "Välj diagnos",
        choices = choices,
        selected = selected,
        sort = FALSE,
        multiple = multiple
    )
}

#' @export
#' Preset picker for outcome
inp_picker_outcome <- function(id, choices, selected = NULL) {
    inp_picker(
        id,
        label = "Välj utfallsmått",
        choices = choices,
        selected = NULL,
        sort = FALSE
    )
}

#' @export
#' Hmm, is this in use?
inp_picker_timestamp <- function(id, choices, unit = NULL) {
    inp_picker(
        id,
        label = paste("Välj jämforelser", unit),
        choices = choices
    )
}
