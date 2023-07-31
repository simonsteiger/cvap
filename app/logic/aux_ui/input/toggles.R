box::use(
    sh = shiny,
)

#' @export
#' Custom toggle switch
inp_toggle <- function(id, label, value = FALSE, class = NULL) {
    if (!is.null(class)) {
        class <- paste("form-check form-switch", class, " ")
    } else {
        class <- "form-check form-switch"
    }

    input_tag <- sh$tags$input(
        class = "form-check-input",
        type = "checkbox",
        role = "switch",
        id = id
    )

    if (!is.null(value) && value) {
        input_tag$attribs$checked <- "checked"
    }

    sh$div(
        class = class,
        input_tag,
        sh$tags$label(
            class = "form-check-label",
            `for` = id,
            label
        )
    )
}
