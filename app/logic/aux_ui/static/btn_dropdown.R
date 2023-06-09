box::use(
    sh = shiny,
    rl = rlang,
)

#' @export
btn_dropdown <- function(...) {
    dots <- rl$list2(...)

    sh$div(
        class = "dropdown",
        sh$tags$button(
            class = "btn btn-secondary dropdown-toggle hover p-2",
            type = "button",
            `data-bs-toggle` = "dropdown",
            `aria-expanded` = "false",
            "Anpassa filtren"
        ),
        sh$tags$ul(
            class = "dropdown-menu",
            sh$tags$li(
                class = "m-2",
                !!!dots
            )
        )
    )
}
