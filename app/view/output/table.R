box::use(
    magrittr[`%>%`],
    sh = shiny,
    dp = dplyr,
    rtbl = reactable,
)

box::use(
    aui = app / logic / aux_ui,
)

#' @export
ui <- function(id) {
    ns <- sh$NS(id)
    aui$card(
        header = sh$div(
            class = "d-flex flex-row align-items-center",
            "Tabell",
            aui$btn_modal(
                ns("info-tabell"),
                label = sh$icon("circle-info"),
                modal_title = "Information om tabell",
                footer_confirm = NULL,
                footer_dismiss = NULL,
                class_toggle = "btn btn-transparent",
                "Infotext om tabell"
            )
        ),
        body = rtbl$reactableOutput(ns("table"))
    )
}

#' @export
server <- function(id, .data, arrange = NULL) {
    sh$moduleServer(id, function(input, output, session) {
        stopifnot(sh$is.reactive(.data))

        output$table <- rtbl$renderReactable(
            rtbl$reactable(
                .data() %>%
                    dp$arrange(dp$across(arrange))
            )
        )
    })
}
