box::use(
    bsl = bslib,
)

#' @export
#' A vanilla navset_card_tab container for VAPs
navset_card_tab <- function(title, sidebar, ...) {
    bsl$navset_card_tab(
        height = 650,
        full_screen = TRUE,
        title = title,
        sidebar = sidebar,
        ...
    )
}