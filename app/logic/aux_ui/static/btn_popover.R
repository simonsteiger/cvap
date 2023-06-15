box::use(
    sh = shiny,
)

btn_popover <- function(title, content) {
    sh$tags$a(
        tabindex = "0",
        class = "btn btn-outline-primary",
        role = "button",
        `data-bs-toggle` = "popover",
        `data-bs-trigger` = "focus",
        `data-bs-title` = title,
        `data-bs-content` = content,
        title
    )
}
