box::use(
    hw = htmlwidgets,
    sh = shiny,
    str = stringr,
    pr = purrr,
    bsl = bslib,
    dp = dplyr,
    rl = rlang[`%||%`],
    lub = lubridate,
    gl = glue,
    magrittr[`%>%`],
)

box::use(
    aui = app / logic / aux_ui,
    app / logic / srqlib / srqdict,
)

# JavaScript formatters ----

#' @export
#' wraps a JavaScript function which accesses name and value parameters and formats them
#' value is expected to be Län
format_decimal <- function() {
    hw$JS("
            function(params){
                return('<b>Län:</b> ' + params.name + '<br /><b>Värde:</b> ' + params.value)
            }
        ")
}

#' @export
#' wraps a JavaScript function which accesses name and value parameters and formats them
#' value is expected to be Län
format_percent <- function() {
    hw$JS("
            function(params){
                return('<b>Län:</b> ' + params.name + '<br /><b>Värde:</b> ' + params.value + ' %')
            }
        ")
}

#' @export
format_year <- function() {
    hw$JS("
        {yyyy}
    ")
}

#' @export
format_riket_bold <- function() {
    hw$JS("
        function (lan) {
            if (lan == 'Riket') {
                return `{b|${lan}}`;
            } else {
                return lan;
            }
        }
      ")
}

#' @export
#' wraps the previously defined formatters so they can be subsetted based on function args
format_list <- list(
    decimal = format_decimal,
    percent = format_percent,
    year = format_year,
    riket = format_riket_bold
)

# ICD formatters ----

icd_helper <- function(icd) {
    pr$map(icd, \(x) sh$tags$li(x)) %>% sh$tags$ul(class = "small")
}

#' @export
icd_compose <- function(icd) {
    switch(str$str_extract(icd$header, "(?<=\\()\\w+(?=\\))"), # get string in brackets
        "RA" = sh$tagList(sh$tags$h6(icd$header), icd_helper(icd$codes)),
        "AS" = sh$tagList(sh$tags$h6(icd$header), icd_helper(icd$codes)),
        "SpA" = sh$tagList(sh$tags$h6(icd$header), icd_helper(icd$codes)),
        "PsA" = sh$tagList(sh$tags$h6(icd$header), icd_helper(icd$codes)),
        stop(paste0("Unknown header (diagnosis) to icd_compose()"))
    )
}

# Icon formatters ----
# This submodule collects several functions which create overview icons from user inputs

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
    dots <- rl$list2(...)
    if (dots$datecompare) {
        sh$div(
            class = "d-flex flex-row align-items-center gap-3",
            sh$icon("calendar"), paste0(unique(lub$year(input)), collapse = " vs ")
        )
    } else {
        sh$div(
            class = "d-flex flex-row align-items-center gap-3",
            sh$icon("calendar"), paste0(input, collapse = " till ")
        )
    }
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
        "inkluderad" = "inklusion",
        stop("Unknown input")
    )
    sh$div(
        class = "d-flex flex-row align-items-center gap-3",
        sh$icon("clock-rotate-left"), paste("Sjukdomsdebut till", translated)
    )
}

icon_prep_typ <- function(input, ...) {
    stopifnot(str$str_detect(input, "dmard|bioprep"))
    translated <- switch(input,
        "bioprep" = "bDMARD",
        "csdmard" = "csDMARD",
        "bDMARD / csDMARD"
    )
    sh$div(
        class = "d-flex flex-row align-items-center gap-3",
        sh$icon("syringe"), translated
    )
}

icon_outcome <- function(input, ...) {
    translated <- switch(input,
        "das28_low" = "DAS28 < 3.2",
        "cdai_low" = "CDAI <= 10",
        "patientens_globala" = "Allmän hälsa",
        "haq" = "HAQ",
        "smarta" = "Smärta",
        "per100k" = "Antal per 100 000",
        "n" = "Total antal",
        stop("Unknown input")
    )
    sh$div(
        class = "d-flex flex-row align-items-center gap-3",
        sh$icon("square-root-variable"), translated # bullseye is good, too
    )
}

icon_lan <- function(input, ...) {
    sh$div(
        class = "d-flex flex-row align-items-center gap-3",
        sh$icon("map-location-dot"), paste0(length(input), " län")
    )
}

icon_lan_modal <- function(input, ...) {
    dots <- rl$list2(...)
    cond_one <- length(unlist(input)) > 1
    cond_all <- length(unlist(input)) == 21 # TODO take care of RIKET for Täckningsgrad
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
icon_samplesmall <- function(id, input, value) {
    sh$div(
        class = "d-flex flex-row justify-content-between align-items-center",
        sh$div(
            class = "d-flex flex-row align-items-center gap-3",
            sh$tags$i(class = "fa fa-users-slash c-warning"),
            paste0("Lite data i ", length(input), " län")
        ),
        aui$inp_toggle(id = id, label = "Dölj", value = value)
    )
}

#' @export
icon_samplecrit <- function(input) {
    sh$div(
        class = "d-flex flex-row align-items-center gap-3",
        sh$tags$i(class = "fa fa-users-slash c-danger"),
        paste0("Otillräckliga data i ", length(input), " län")
    )
}

#' @export
iconostasis <- list(
    kon = icon_kon,
    alder = icon_alder,
    inkluderad = icon_date,
    ordinerat = icon_date, # could the issue arise with several date icons?
    ongoing = icon_date,
    year = icon_date,
    lan = icon_lan, # can switch to modal if need be
    dxcat = icon_dxcat,
    start = icon_start,
    prep_typ = icon_prep_typ,
    outcome = icon_outcome,
    samplesmall = icon_samplesmall,
    samplecrit = icon_samplecrit
)

# Input-to-caption formatters ----

# This submodule collects functions for translating the filter input to summary text
# on the downloadable plots

spell_kon <- function(x) {
    x %||% return("")

    if (x == "Båda") {
        "för båda kön"
    } else if (x == "Man") {
        "för män"
    } else {
        "för kvinnor"
    }
}

spell_alder <- function(x) {
    if (!is.null(x)) {
        gl$glue("mellan {x[1]} och {x[2]} år")
    } else {
        ""
    }
}

spell_outcome <- function(x) {
    dp$case_match(
        x,
        "das28_low" ~ "percent patienter med DAS28 < 3.2",
        "cdai_low" ~ "percent patienter med CDAI <= 10",
        "patientens_globala" ~ "medianvärde av allmän hälsa",
        "haq" ~ "medianvärde av HAQ",
        "smarta" ~ "medianvärde av smärta",
        "per100k" ~ "antal utvalda patienter per 100.000 invånare",
        "n" ~ "antal utvalda patienter",
        .default = tolower(x)
    ) %>%
        sh$tags$b()
}

spell_period <- function(x, datecompare) {
    date <- x$inkluderad %||% x$ordinerat %||% x$ongoing %||% return("")

    # Set correct "context" depending if two years are compared or a timeframe is selected
    if (datecompare) {
        date_context <- c("år", "jämfort med år")
        date <- format(date, format = "%Y")
    } else {
        date_context <- c("från", "till")
    }

    if (length(date) == 2) {
        gl$glue("{date_context[1]} {date[1]} {date_context[2]} {date[2]}")
    } else if (is.null(x$lookback)) { # assume that there is a lookback input
        gl$glue("från {date}")
    } else {
        x$lookback %||% stop("Require input$lookback to generate period text")
        gl$glue("från {date-lub$years(x$lookback)} till {date}")
    }
}

spell_dxcat <- function(x) {
    x %||% return("")
    x[x %in% "Annan"] <- "en diagnos inom kategori 'Annan'"
    gl$glue("diagnosticerad med {paste0(x, collapse = ', ')}")
}

spell_prep_typ <- function(x) {
    x %||% return("")
    x <- dp$case_match(
        x,
        "Båda" ~ "antingen csDMARDs eller bDMARDs",
        "bioprep" ~ "bDMARDs",
        "csdmard" ~ "csDMARDs"
    )
    gl$glue("behandlade med {x}")
}

#' @export
create_subtitle <- function(input, .var, datecompare) {
    paste0(
        paste(
            "Denna graf visa",
            spell_outcome(input$outcome %||% .var),
            spell_kon(input$kon),
            spell_alder(input$alder),
            spell_period(input, datecompare),
            # can't use `[` to index reactive [pr$map_lgl(input, lub$is.Date)]
            sep = " "
        ), ".",
        if (!is.null(input$dxcat) || !is.null(input$prep_typ)) {
            paste0(paste(
                " Alla personer är",
                spell_dxcat(input$dxcat),
                if (is.null(input$dxcat)) "" else "och",
                spell_prep_typ(input$prep_typ),
                sep = " "
            ), ".")
        }
    )
}

#' @export
create_title_suffix <- function(input, title) {
    if (is.null(input$dxcat)) {
        return(title)
    } else if (!"Tidig RA" %in% input$dxcat) {
        return(paste(title, "i månader"))
    } else if ("Tidig RA" %in% input$dxcat) {
        paste(title, "i veckor")
        return(paste(title, "i veckor"))
    } else {
        stop("no match for create_title_suffix")
    }
}

#' @export
translate_outcome <- function(chr_vec, ...) {
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
            "n" = "Total antal",
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
