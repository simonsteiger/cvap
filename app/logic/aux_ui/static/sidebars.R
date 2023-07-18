box::use(
    sh = shiny,
    bsl = bslib,
    rl = rlang[`%||%`],
)

box::use(
    app / logic / aux_ui / static / btn_modal[...]
)

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
sidebar_filter <- function(id_modal, id_overview, ..., footer_summary = NULL) {
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
                footer_summary = footer_summary
            )
        ),
        body = sh$htmlOutput(id_overview)
    )
}
