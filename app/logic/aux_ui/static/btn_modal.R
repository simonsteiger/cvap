box::use(
    sh = shiny,
    rl = rlang,
)

#' @export
btn_modal <- function(id, title, label_confirm = "Save changes", label_close = "Close", ...) {
    dots <- rl$list2(...)

    sh$div(
        sh$tags$button(
            class = "btn btn-secondary hover p-2",
            type = "button",
            `data-bs-toggle` = "modal",
            `data-bs-target` = paste("#inputModal", id, sep = "-"),
            "Anpassa filtren"
        ),
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
                        class = "modal-header",
                        sh$tags$h1(
                            class = "modal-title fs-5",
                            id = paste("inputModalLabel", id, sep = "-"),
                            title
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
                        class = "modal-footer",
                        sh$tags$button(
                            type = "button",
                            class = "btn btn-secondary hover p-2",
                            `data-bs-dismiss` = "modal",
                            label_close
                        ),
                        sh$tags$button(
                            id = id,
                            type = "button",
                            class = "btn btn-success action-button hover-success p-2",
                            `data-bs-dismiss` = "modal",
                            label_confirm
                        )
                    )
                )
            )
        )
    )
}
