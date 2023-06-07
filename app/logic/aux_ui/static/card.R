box::use(
    bsl = bslib,
)

#' @export
#' A vanilla card container for VAPs
card <- function(header = NULL, body = NULL, footer = NULL) {
    bsl$card(
        bsl$card_header(header),
        bsl$card_body(body),
        bsl$card_footer(footer),
        min_height = 650,
        full_screen = TRUE
    )
}
