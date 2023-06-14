box::use(
    sh = shiny,
    bsl = bslib,
)

#' @export
sidebar <- function(title = "Översikt", header = NULL, body = NULL, footer = NULL) {
    bsl$card(
        # class = "my-3",
        height = "650px",
        bsl$card_header(
            sh$div(
                class = "d-flex justify-content-between align-items-center",
                title,
                header
            )
        ),
        bsl$card_body(body),
        bsl$card_footer(footer)
    )
}
