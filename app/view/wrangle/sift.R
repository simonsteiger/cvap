box::use(
    sh = shiny,
    pr = purrr,
    shw = shinyWidgets,
    shf = shinyFeedback,
    shj = shinyjs,
    dp = dplyr,
    lub = lubridate,
    ts = tidyselect,
    magrittr[`%>%`],
    rl = rlang[`%||%`],
    str = stringr,
)

box::use(
    aui = app / logic / aux_ui,
    ase = app / logic / aux_server,
)

#' @export
ui <- function(id, id_go_input, id_overview, inputs) {
    ns <- sh$NS(id)
    aui$sidebar_filter(
        id_go_input, id_overview,
        inputs,
        modal_summary = sh$htmlOutput(ns("feedback"))
    )
}

#' @export
server <- function(id, data, .var = NULL, button = TRUE, datecompare = FALSE) {
    sh$moduleServer(id, function(input, output, session) {
        stopifnot(sh$is.reactive(data))

        # This observer only exists if there are timestamp and dxcat inputs
        # Sets timestamp choices for dxcat on inklusionsmatt_1
        sh$observeEvent(input$dxcat, {
            if (!is.null(input$timestamp) && !is.null(input$dxcat)) {
                choices <-
                    data() %>%
                    `$`(., timestamp) %>% # get timestamp vector
                    `[`(., data()$dxcat %in% input$dxcat) %>% # subset by cond (arg #2 of `[`)
                    droplevels() %>% # drop unused levels
                    levels() # return remaining levels as vector of strings

                unit <- if (input$dxcat == "Tidig RA") "(i veckor)" else "(i månader)"
                # Update label, choices and selected according to selected dxcat
                shw$updatePickerInput(
                    session,
                    "timestamp",
                    label = paste("Välj jämförelser", unit),
                    choices = choices,
                    selected = choices
                )
            }
        })

        # Create bool filter vector
        sieve <- ase$sift_vars(data, input)

        # Check if any of the existing inputs is NULL
        # Iterate over input names, use these to index into input and check if each value is NULL
        # Then collapse resulting bool vector into single bool with `|`
        any_input_null <- sh$reactive({
            null_inputs <- pr$map_lgl(names(input), \(x) is.null(input[[x]]))
            pr$reduce(null_inputs, `|`)
        })

        # Basic filter with sieve vector
        out <- sh$reactive({
            if (!ase$vali_date(input)$inrange || any_input_null()) {
                dp$filter(data(), FALSE) # return empty tibble
            } else {
                data()[sieve(), ] %>%
                    ase$maybe_lookback(input, .var) %>%
                    ase$maybe_ongoing(input) %>%
                    ase$maybe_datecompare(input, datecompare)
            }
        })

        feedback <- sh$reactive(ase$sift_feedback(out(), input, .var, button))

        # Create user feedback (warnings and case number summary)
        output$feedback <- sh$renderUI(feedback())

        out
    })
}
