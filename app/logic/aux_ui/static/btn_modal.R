box::use(
    sh = shiny,
    rl = rlang,
)

#' @export
btn_modal <- function(id, title, confirm_btn_name = "Save changes", close_btn_name = "Close", ...) {
    dots <- rl$list2(...)

    sh$div(
        sh$tags$button(
            class = "btn btn-secondary hover p-2",
            type = "button",
            `data-bs-toggle` = "modal",
            `data-bs-target` = "#inputModal",
            "Anpassa filtren"
        ),
        sh$div(
            class = "modal fade",
            id = "inputModal",
            tabindex = "-1",
            `aria-labelledby` = "inputModalLabel",
            `aria-hidden` = "true",
            sh$div(
                class = "modal-dialog",
                sh$div(
                    class = "modal-content",
                    sh$div(
                        class = "modal-header",
                        sh$tags$h1(
                            class = "modal-title fs-5",
                            id = "inputModalLabel",
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
                            close_btn_name
                        ),
                        sh$tags$button(
                            id = id,
                            type = "button",
                            class = "btn btn-success action-button hover p-2",
                            `data-bs-dismiss` = "modal",
                            confirm_btn_name
                        )
                    )
                )
            )
        )
    )
}
