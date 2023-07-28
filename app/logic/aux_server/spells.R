box::use(
    gl = glue,
    rl = rlang[`%||%`],
    dp = dplyr,
    sh = shiny,
    pr = purrr,
    lub = lubridate,
    magrittr[`%>%`],
)

# This module collects functions for translating the filter input to summary text
# on the downloadable plots

#' @export
spell_kon <- function(x) {
    x %||% return(NULL)

    if (x == "Båda") {
        "för båda kön"
    } else if (x == "Man") {
        "för män"
    } else {
        "för kvinnor"
    }
}

#' @export
spell_alder <- function(x) {
    if (!is.null(x)) {
        gl$glue("mellan {x[1]} och {x[2]} år")
    } else {
        NULL
    }
}

#' @export
spell_outcome <- function(x) {
    dp$case_match(
        x,
        "das28_low" ~ "andelen patienter med DAS28 < 3.2",
        "cdai_low" ~ "andelen patienter med CDAI <= 10",
        "patientens_globala" ~ "medianvärde av allmän hälsa",
        "haq" ~ "medianvärde av HAQ",
        "smarta" ~ "medianvärde av smärta",
        .default = tolower(x)
    ) %>%
        sh$tags$b()
}

#' @export
spell_period <- function(x) {
    date <- x$inkluderad %||% x$ordinerat %||% x$ongoing %||% return(NULL)

    if (length(date) == 2) {
        gl$glue("från {date[1]} till {date[2]}")
    } else if (is.null(x$lookback)) { # assume that there is a lookback input
        gl$glue("från {date}")
    } else {
        x$lookback %||% stop("Require input$lookback to generate period text")
        gl$glue("från {date-lub$years(x$lookback)} till {date}")
    }
}

#' @export
spell_dxcat <- function(x) {
    x %||% return(NULL)
    x[x %in% "Annan"] <- "en diagnos inom kategori 'Annan'"
    gl$glue("diagnosticerad med {paste0(x, collapse = ', ')}")
}

#' @export
spell_prep_typ <- function(x) {
    x %||% return(NULL)
    x <- if (x == "Båda") "antingen csDMARD eller bDMARD"
    gl$glue("och behandlad med {x}")
}

#' @export
create_subtitle <- function(input, .var) {
    paste0(
        paste(
            "Denna graf visa",
            spell_outcome(input$outcome %||% .var),
            spell_kon(input$kon),
            spell_alder(input$alder),
            spell_period(input), # can't use `[` to index reactive [pr$map_lgl(input, lub$is.Date)]
            sep = " "
        ), ".",
        if (!is.null(input$dxcat) || !is.null(input$prep_typ)) {
            paste0(paste(
                " Alla personer är",
                spell_dxcat(input$dxcat),
                spell_prep_typ(input$prep_typ),
                sep = " "
            ), ".")
        }
    )
}
