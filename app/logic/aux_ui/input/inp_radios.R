box::use(
    sh = shiny,
    sw = shinyWidgets,
)

#' @export
inp_radio <- function(id, label, choices, selected = NULL) {
    sh$div(
        class = "mb-3 mx-1",
        sw$prettyRadioButtons(
            inputId = id,
            label = label,
            status = "primary",
            shape = "round",
            animation = "smooth",
            plain = TRUE,
            inline = TRUE,
            choices = choices,
            selected = selected
        )
    )
}

#' @export
inp_radio_sex <- function(id) {
    inp_radio(
        id = id,
        label = "Välj kön",
        choices = c("Kvinna", "Man", "Båda"),
        selected = "Båda"
    )
}

#' @export
inp_radio_lookback <- function(id) {
    inp_radio(
        id = id,
        label = "Välj period",
        choices = c(
            "ett år" = 1,
            "två år" = 2,
            "tre år" = 3
        )
    )
}

#' @export
inp_radio_outcome <- function(id, choices) {
    inp_radio(
        id = id,
        label = "Välj utfallsmått",
        choices = choices
    )
}

#' @export
choices <- list(
    das28_cdai = c("DAS28 < 3.2" = "das28_low", "CDAI <= 10" = "cdai_low"),
    glob_haq_smarta = c("Allmän hälsa" = "patientens_globala", "HAQ" = "haq", "Smärta" = "smarta")
)
