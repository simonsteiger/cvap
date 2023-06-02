box::use(
    sh = shiny,
    bsl = bslib,
    rt = shiny.router,
)

#' @export
return_button <- function(id) {
    sh$actionButton(id, label = "Tillbaka", icon = sh$icon("angles-left"), class = "hover p-2")
}

#' @export
#' A vanilla navset_card_tab container for VAPs
vap_card <- function(title, ...) {
    bsl$navset_card_tab(
        height = 650,
        full_screen = TRUE,
        title = title,
        ...
    )
}

#' @export
container_fluid <- function(...) {
    sh$div(
        class = "container-fluid m-5",
        ...
    )
}

#' @export
row <- function(left = NULL, center = NULL, right = NULL, colwidths = c(2, 8, 2)) {
    cols <- paste0("col-", colwidths)
    sh$div(
        class = "row",
        sh$div(
            class = cols[1],
            left,
        ),
        sh$div(
            class = cols[2],
            center,
        ),
        sh$div(
            class = cols[3],
            right,
        )
    )
}

#' @export
head <- function() {
    sh$div(class = "h1 text-center", "Visualiserings- och analysplattform")
}

#' @export
logo <- function() {
    sh$div(
        class = "h3 d-flex justify-content-center",
        "SRQ Logo"
    )
}