box::use(
    sh = shiny,
    rl = rlang,
)

#' @export
btn_return <- function(id) {
    sh$actionButton(
        id,
        label = "Tillbaka",
        icon = sh$icon("angles-left"),
        class = "hover"
    )
}

#' @export
#' Download the object with `id` as CSV
btn_download_csv <- function(id) {
    sh$tags$button(
        class = "btn btn-secondary hover",
        sh$icon("download"), "Download",
        onclick = paste0("Reactable.downloadDataCSV('", ns("table"), "')")
    )
}

#' @export
btn_modal <- function(id, label, modal_title, footer_confirm = NULL, footer_dismiss = NULL, ..., modal_summary = NULL) {
    dots <- rl$list2(...)

    if (!is.null(footer_dismiss)) {
        footer_dismiss <- sh$tags$button(
            type = "button",
            class = "btn btn-secondary hover",
            `data-bs-dismiss` = "modal",
            footer_dismiss
        )
    }

    if (!is.null(footer_confirm)) {
        footer_confirm <- sh$tags$button(
            id = id,
            type = "button",
            class = "btn btn-success action-button hover-success",
            `data-bs-dismiss` = "modal",
            footer_confirm
        )
    }

    if (!is.null(dots$class_toggle)) {
        toggle <- sh$tags$button(
            id = paste("openModal", id, sep = "-"),
            class = dots$class_toggle,
            type = "button",
            `data-bs-toggle` = "modal",
            `data-bs-target` = paste("#inputModal", id, sep = "-"),
            label
        )
    } else {
        toggle <- sh$tags$button(
            id = paste("openModal", id, sep = "-"),
            class = "btn btn-secondary hover",
            type = "button",
            `data-bs-toggle` = "modal",
            `data-bs-target` = paste("#inputModal", id, sep = "-"),
            label
        )
    }

    sh$div(
        toggle,
        sh$div(
            class = "modal fade",
            id = paste("inputModal", id, sep = "-"),
            tabindex = "-1",
            `aria-labelledby` = paste("inputModalLabel", id, sep = "-"),
            `aria-hidden` = "true",
            sh$div(
                class = "modal-dialog",
                sh$div(
                    class = "modal-content",
                    sh$div(
                        class = "bg-secondary modal-header",
                        sh$tags$h1(
                            class = "modal-title fs-5",
                            id = paste("inputModalLabel", id, sep = "-"),
                            modal_title
                        ),
                        sh$tags$button(
                            type = "button",
                            class = "btn-close",
                            `data-bs-dismiss` = "modal",
                            `aria-label` = "Close"
                        ),
                    ),
                    sh$div(
                        class = "modal-body",
                        !!!dots
                    ),
                    sh$div(
                        class = "modal-footer justify-content-between",
                        sh$div(modal_summary),
                        #sh$tags$button(type = "button", class = "btn btn-primary", "that's it"),
                        sh$div(
                            footer_dismiss,
                            footer_confirm
                        )
                    )
                )
            )
        )
    )
}
