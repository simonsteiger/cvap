box::use(
    sh = shiny,
    gg = ggplot2,
    gl = glue,
    dp = dplyr,
    pr = purrr,
    rl = rlang[`%||%`],
)

spell_kon <- function(x) {
    gl$glue("Kön: {x}")
}

spell_age <- function(x) {
    gl$glue("Ålder: {x[1]}-{x[2]} år")
}

spell_outcome <- function(x) {
    y <- dp$case_match(
        x,
        "das28_low" ~ "DAS28 < 3.2",
        "cdai_low" ~ "CDAI <= 10",
        "patientens_globala" ~ "Allmän hälsa",
        "haq" ~ "HAQ",
        "smarta" ~ "Smärta"
    )

    if (is.na(y)) stop("Unknown x to spell_outcome")

    gl$glue("Utfallsmått: {y}")
}

spell_lookback <- function(x) {
    gl$glue("Period: börjar {x} år innan valt datum")
}

spell_date <- function(x) {
    gl$glue("Datum: {x}")
}

spell_daterange <- function(x) {
    gl$glue("Tidsfönster: {x[1]} till {x[2]}")
}

spellbook <- list(
    alder = spell_age,
    kon = spell_kon,
    outcome = spell_outcome
)

#' @export
server <- function(id, .var) {
    sh$moduleServer(id, function(input, output, session) {
        subtitle <- sh$reactive({
            inputs_with_spells <- names(input)[names(input) %in% names(spellbook)]

            pr$map_chr(
                inputs_with_spells,
                \(name) {
                    spellbook[[name]](input[[name]])
                }
            )
        })

        sh$reactive({
            list(
                caption = paste0(subtitle(), collapse = "   "),
                outcome = input$outcome %||% .var
            )
        })
    })
}
