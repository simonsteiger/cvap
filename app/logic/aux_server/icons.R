box::use(
    sh = shiny,
    bsl = bslib,
    dp = dplyr,
    rl = rlang,
    pr = purrr,
)

box::use(
    aui = app / logic / aux_ui,
)

icon_kon <- function(input, ...) {
    sh$div(
        class = "d-flex flex-row align-items-center gap-3",
        switch(input,
            "Kvinna" = sh$tagList(sh$icon("venus")),
            "Man" = sh$tagList(sh$icon("mars")),
            sh$tagList(sh$icon("venus-mars"))
        ),
        input
    )
}

icon_alder <- function(input, ...) {
    sh$div(
        class = "d-flex flex-row align-items-center gap-3",
        dp$case_when(
            max(input) < 40 ~ sh$tagList(sh$icon("children")),
            min(input) > 70 ~ sh$tagList(sh$icon("person-cane")),
            .default = sh$tagList(sh$icon("people-group"))
        ),
        paste0(paste0(input, collapse = " till "), " år")
    )
}

icon_date <- function(input, ...) {
    sh$div(
        class = "d-flex flex-row align-items-center gap-3",
        sh$icon("calendar"), paste0(input, collapse = " till ")
    )
}

icon_lan <- function(input, ...) {
    sh$div(
        class = "d-flex flex-row align-items-center gap-3",
        sh$icon("map-location-dot"), paste0(input, collapse = ", ")
    )
}

icon_dxcat <- function(input, ...) {
    sh$div(
        class = "d-flex flex-row align-items-center gap-3",
        sh$icon("clipboard-list"), paste0(input, collapse = ", ")
    )
}

icon_start <- function(input, ...) {
    translated <- switch(input,
        "min_inkl_diag" = "inklusion eller diagnos",
        "diagnosdatum1" = "diagnos",
        "inkluderad" = "inklusion"
    )
    sh$div(
        class = "d-flex flex-row align-items-center gap-3",
        sh$icon("clock-rotate-left"), paste("Sjukdomsdebut till", translated)
    )
}

icon_lan_modal <- function(input, ...) {
    dots <- rl$list2(...)
    cond_one <- length(unlist(input)) > 1
    cond_all <- length(unlist(input)) == 21
    res <- pr$map(input, \(x) sh$tags$li(x))

    sh$tagList(
        sh$div(
            class = "d-flex justify-content-between align-items-center",
            sh$div(
                class = "d-flex flex-row align-items-center gap-3",
                sh$icon("map-location-dot"),
                paste0(
                    if (cond_all) "Alla" else length(input),
                    " län"
                )
            ),
            aui$btn_modal(
                id = dots$id,
                label = sh$tagList(sh$icon("list-ul"), "Se lista"),
                modal_title = if (cond_one) "Utvalda län" else "Utvalt län",
                footer_confirm = NULL,
                footer_dismiss = NULL,
                sh$div(
                    class = "m-2",
                    bsl$layout_column_wrap(
                        width = 1 / 3, height = 300,
                        !!!res
                    )
                )
            )
        )
    )
}

#' @export
iconostasis <- list(
    kon = icon_kon,
    alder = icon_alder,
    inkluderad = icon_date,
    ordinerat = icon_date,
    ongoing = icon_date,
    lan = icon_lan_modal,
    dxcat = icon_dxcat,
    start = icon_start
)