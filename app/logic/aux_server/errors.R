box::use(
    shw = shinyWidgets,
)

#' @export
#' Wrapper with presets of shinyWidgets' show_alert
#' Currently the only way to see this error is if all remaining lans are small lans
#' and are then hidden by the user
error_no_data <- function(session) {
    shw$show_alert(
        session = session,
        title = "Fel: ingen data",
        text = "Anpassa filtren och försök igen.",
        type = "error",
        btn_labels = "Stäng",
    )
}