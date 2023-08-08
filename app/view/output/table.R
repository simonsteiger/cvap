box::use(
    magrittr[`%>%`],
    sh = shiny,
    dp = dplyr,
    rtbl = reactable,
)

box::use(
    app / logic / swissknife / sklang[`%//%`],
    aui = app / logic / aux_ui,
    ase = app / logic / aux_server,
    app / logic / data / summaries[info_table]
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
                info_table
            )
        ),
        body = rtbl$reactableOutput(ns("table")),
        footer = sh$div(
            class = "d-flex flex-row align-items-center justify-content-start",
            sh$tags$button(class = "btn btn-secondary hover", sh$icon("download"), "Download", onclick = paste0("Reactable.downloadDataCSV('", ns("table"), "')"))
        )
    )
}

#' @export
server <- function(id, .data, stash = NULL, arrange = NULL) {
    sh$moduleServer(id, function(input, output, session) {
        stopifnot(sh$is.reactive(.data))
        stopifnot(sh$is.reactive(stash))

        output$table <- rtbl$renderReactable({
            if (nrow(.data()) > 0 && all(is.na(.data()$outcome))) ase$error_no_data(session)
            sh$req(nrow(.data()) > 0 && !all(is.na(.data()$outcome)))

            # Here say "if data contains dxcat == Tidig RA, then make timestamp '... i veckor"
            rtbl$reactable({
                temp <- dp$arrange(.data(), dp$across(arrange))

                # if dxcat is Tidig RA, custom rename - always FALSE if dxcat doesn't exist
                # convert from tibble to data.frame to avoid "uninitialised column" warning
                if (all(as.data.frame(.data())$dxcat %in% "Tidig RA" %//% FALSE)) {
                    temp <- dp$rename(temp, `Veckor efter sjukdomsdebut` = timestamp)
                }

                temp %>%
                    dp$rename(!!stash()$outcome := outcome) %>%
                    dp$rename_with(ase$translate_outcome)
            })
        })
    })
}
