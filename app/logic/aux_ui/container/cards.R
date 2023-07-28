box::use(
    sh = shiny,
    bsl = bslib,
    rl = rlang[`%||%`],
)

box::use(
    app / logic / aux_ui / input / inp_buttons[btn_modal]
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

#' @export
sidebar <- function(header = NULL, body = NULL, ...) {
    dots <- rl$list2(...)

    bsl$card(
        height = "317px",
        bsl$card_header(header, ...),
        bsl$card_body(body)
    )
}

#' @export
sidebar_filter <- function(id_modal, id_overview, ..., modal_summary = NULL) {
    sidebar(
        header = sh$div(
            class = "d-flex justify-content-between align-items-center",
            "Översikt",
            btn_modal(
                id_modal,
                label = sh$tagList(sh$icon("filter"), "Anpassa"),
                modal_title = "Filtermeny",
                footer_confirm = "Bekräfta",
                footer_dismiss = "Avbryt",
                ...,
                modal_summary = modal_summary
            )
        ),
        body = sh$htmlOutput(id_overview)
    )
}
