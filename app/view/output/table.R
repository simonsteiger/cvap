box::use(
    magrittr[`%>%`],
    sh = shiny,
    dp = dplyr,
    rtbl = reactable,
    rl = rlang,
    pr = purrr,
)

box::use(
    swissknife / sklang[`%//%`],
    aui = app / logic / aux_ui,
    ase = app / logic / aux_server,
)

translate <- function(chr_vec, ...) {
    dots <- rl$list2(...)

    pr$map_chr(chr_vec, \(chr) {
        switch(chr,
            "lan" = "Lan",
            "inkluderad" = "Inklusion",
            "ordinerat" = "Ordination",
            "ongoing_timestamp" = "Pågående vid",
            "visit_group" = "Tidpunkt",
            "dxcat" = "Diagnos kategori",
            "patientens_globala" = "Allmän hälsa",
            "haq" = "HAQ",
            "smarta" = "Smärta",
            "das28_low" = "Låg DAS28",
            "cdai_low" = "Låg CDAI",
            "per100k" = "Antal per 100_000",
            "nonmissing" = "Data tillgänglig",
            "missing" = "Data saknas",
            "population" = "Population",
            "year" = "År",
            "timestamp" = "Månader efter sjukdomsdebut", # Adjust this for tidig RA
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
                    dp$rename_with(translate)
            })
        })
    })
}
