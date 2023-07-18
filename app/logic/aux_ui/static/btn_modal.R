box::use(
    sh = shiny,
    rl = rlang,
)

#' @export
btn_modal <- function(id, label, modal_title, footer_confirm = NULL, footer_dismiss = NULL, ..., footer_summary = NULL) {
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
            class = dots$class_toggle,
            type = "button",
            `data-bs-toggle` = "modal",
            `data-bs-target` = paste("#inputModal", id, sep = "-"),
            label
        )
    } else {
        toggle <- sh$tags$button(
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
                        sh$div(footer_summary),
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
