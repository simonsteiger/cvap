box::use(
    magrittr[`%>%`],
    sh = shiny,
    dp = dplyr,
    rtbl = reactable,
    rl = rlang,
    pr = purrr,
)

box::use(
    aui = app / logic / aux_ui,
)

translate <- function(chr_vec, ...) {
    dots <- rl$list2(...)

    pr$map_chr(chr_vec, \(chr) {
        switch(chr,
            "inkluderad" = "Inklusions",
            "ordinerat" = "Ordinations",
            "ongoing_timestamp" = "Pågående vid",
            "dxcat" = "Diagnos kategori",
            "patientens_globala" = "Allmän hälsa",
            "haq" = "HAQ",
            "smarta" = "Smärta",
            "das28_low" = "Låg DAS28",
            "cdai_low" = "Låg CDAI",
            chr
        )
    })
}

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
server <- function(id, .data, stash = NULL, arrange = NULL) {
    sh$moduleServer(id, function(input, output, session) {
        stopifnot(sh$is.reactive(.data))
        stopifnot(sh$is.reactive(stash))

        output$table <- rtbl$renderReactable({
            sh$req(nrow(.data()) > 0)

            rtbl$reactable(
                .data() %>%
                    dp$arrange(dp$across(arrange)) %>%
                    dp$rename(!!stash()$outcome := outcome) %>%
                    dp$rename_with(translate)
            )
        })
    })
}
