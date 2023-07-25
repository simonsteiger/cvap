box::use(
    sh = shiny,
    pr = purrr,
    shw = shinyWidgets,
    shf = shinyFeedback,
    shj = shinyjs,
    dp = dplyr,
    lub = lubridate,
    magrittr[`%>%`],
    rl = rlang[`%||%`],
    str = stringr,
)

box::use(
    ase = app / logic / aux_server,
)

#' @export
ui <- function(id) {
    ns <- sh$NS(id)
    sh$tagList(
        sh$textOutput(ns("no_lan")), # output of warning when no lan selected
    )
}

#' @export
server <- function(id, data, .var = NULL) {
    sh$moduleServer(id, function(input, output, session) {
        stopifnot(sh$is.reactive(data))

        # This observer only exists if there are timestamp and dxcat inputs
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

        # Send a warning to user if no läns selected
        no_lan <- sh$reactive(
            shf$feedbackDanger(
                "lan",
                is.null(input$lan),
                "Välj minst ett län.",
                session = session
            )
        )

        # Create bool filter vector
        sieve <- ase$sift_vars(data, input)

        # Basic filter with sieve vector
        out <- sh$reactive({
            if (!ase$vali_date(input)$inrange %||% FALSE) {
                dp$filter(data(), FALSE) # return empty tibble
            } else {
                data()[sieve(), ] %>%
                    ase$maybe_lookback(input, .var) %>%
                    ase$maybe_ongoing(input)
            }
        })

        # check rows of data frame unless lan is NULL (NULL means no filter in sift_vars)
        # if not NULL, count
        # else, 0
        n_cases <- sh$reactive({
            check_date <- ase$vali_date(input)

            shf$feedbackDanger(
                check_date$var,
                !check_date$inrange,
                "Välj två olika datum mellan 1999 och idag.",
                icon = NULL
            )

            if (!is.null(input$lan) && nrow(out()) > 0) {
                n <- sum(!is.na(out()[[input$outcome %||% .var]]))
            } else {
                n <- 0
            }

            sh$tags$button(
                type = "button",
                style = "pointer-events: none;",
                class = ifelse(n > 0, "btn btn-secondary", "btn btn-danger"),
                sh$div(
                    class = "d-flex flex-row align-items-center gap-2",
                    if (n == 0) sh$icon("users-slash") else sh$icon("users"),
                    if (n == 0) "Ingen data, anpassa urval" else paste0("Antal observationer: ", n)
                )
            )
        })

        output$no_lan <- sh$renderText(no_lan())

        output$n_cases <- sh$renderUI(n_cases())

        out
    })
}
