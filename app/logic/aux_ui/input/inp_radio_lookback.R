box::use(
    sh = shiny,
    sw = shinyWidgets,
    lub = lubridate,
)

#' @exportr
inp_radio_lookback <- function(id) {
    sh$div(
        class = "mb-3 mx-1",
        sw$prettyRadioButtons(
            inputId = id,
            label = "Välj period",
            status = "primary",
            shape = "round",
            animation = "smooth",
            plain = TRUE,
            inline = TRUE,
            choices = c(
                "ett år" = 1,
                "två år" = 2,
                "tre år" = 3
            )
        )
    )
}
