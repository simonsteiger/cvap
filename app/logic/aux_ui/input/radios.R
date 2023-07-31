box::use(
    sh = shiny,
    sw = shinyWidgets,
)

#' @export
#' Default wrapper of shinyWidgets' prettyRadioButtons with presets
inp_radio <- function(id, label, choices, selected = NULL) {
    sh$div(
        class = "mb-4 mx-1",
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
#' Preset radio for sex
inp_radio_sex <- function(id) {
    inp_radio(
        id = id,
        label = "Välj kön",
        choices = c("Kvinna", "Man", "Båda"),
        selected = "Båda"
    )
}

#' @export
#' Preset radio for lookback time window
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
#' Preset radio for outcome
inp_radio_outcome <- function(id, choices) {
    inp_radio(
        id = id,
        label = "Välj utfallsmått",
        choices = choices
    )
}

#' @export
#' Gather potential options for outcome measures in a list
#' Just for convenience when passing them in the UI later
choices <- list(
    das28_cdai = c("DAS28 < 3.2" = "das28_low", "CDAI <= 10" = "cdai_low"),
    glob_haq_smarta = c("Allmän hälsa" = "patientens_globala", "HAQ" = "haq", "Smärta" = "smarta")
)

#' @export
#' Preset radio for preparat typ
inp_radio_prep_typ <- function(id) {
    inp_radio(
        id = id,
        label = "Välj typ av behandling",
        choices = c("bDMARD" = "bioprep", "csDMARD" = "csdmard", "Antingen eller" = "Båda")
        # Båda is a free pass through the filters
    )
}

#' @export
#' Preset radio for start variable
inp_radio_start <- function(id) {
    inp_radio(
        id = id,
        label = "Välj duration från sjukdomsdebut till",
        choices = c(
            "Inklusion eller diagnos" = "min_inkl_diag",
            "Inklusion" = "inkluderad",
            "Diagnos" = "diagnosdatum1"
        )
    )
}

#' @export
#' Preset radio for dxcat
inp_radio_dxcat <- function(id, choices) {
    inp_radio(
        id = id,
        label = "Välj diagnos",
        choices = choices
    )
}
