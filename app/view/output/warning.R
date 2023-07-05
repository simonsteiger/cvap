box::use(
    magrittr[`%>%`],
    dp = dplyr,
    sh = shiny,
    rl = rlang,
)

box::use(
    aui = app / logic / aux_ui,
)

icon_samplesize <- function(input, ...) {
    sh$div(
        class = "d-flex flex-row align-items-center gap-2",
        sh$tags$i(class = "fa fa-triangle-exclamation icon-danger"),
        paste0("Mindre än 10 observationer: ", paste0(input, collapse = ", "))
    )
}

icon_samplesize_modal <- function(input, ...) {
    dots <- rl$list2(...)

    if (length(input) == 0) {
        return("Inga varningar")
    } else {
        aui$btn_modal(
            id = dots$id,
            class_toggle = "btn btn-secondary hover",
            label = sh$div(
                class = "d-flex flex-row align-items-center gap-2",
                sh$tags$i(class = "fa fa-triangle-exclamation icon-danger"),
                "Se län med få obs"
            ),
            modal_title = "Varningar",
            footer_confirm = NULL,
            footer_dismiss = NULL,
            sh$div(
                paste0("Mindre än 10 observationer: ", paste0(input, collapse = ", "))
            )
        )
    }
}

#' @export
server <- function(id, .data) {
    sh$moduleServer(id, function(input, output, session) {
        stopifnot(sh$is.reactive(.data))

        sh$reactive({
            .data() %>%
                dp$filter(nonmissing < 10) %>%
                dp$pull(lan) %>%
                unique() %>%
                icon_samplesize_modal(id = id)
        })
    })
}
