box::use(
    shw = shinyWidgets,
)

#' @export
error_no_data <- function(session) {
    shw$show_alert(
        session = session,
        title = "Fel: ingen data",
        text = "Anpassa filtren och försök igen.",
        type = "error",
        btn_labels = "Stäng",
    )
}