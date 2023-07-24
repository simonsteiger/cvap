box::use(
    sh = shiny,
    pr = purrr,
    shw = shinyWidgets,
    shf = shinyFeedback,
    magrittr[`%>%`],
    rl = rlang[`%||%`],
)

box::use(
    ase = app / logic / aux_server,
    app / view / wrangle / lookback,
    app / view / wrangle / ongoing,
    srqlib / srqprep,
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
        dat_basic <- sh$reactive(data()[sieve(), ])

        # If there is a lookback filter, apply that to sieved data
        dat_lookback <- sh$reactive({
            if (!is.null(input$lookback)) {
                res <- lookback$server(id, dat_basic, .var = .var %||% input$outcome)
                res() # lookback returns a reactive, but we want to store its value
            } else {
                dat_basic()
            }
        })

        # If there is an ongoing filter, apply that to sieved data
        dat_ongoing <- sh$reactive({
            if (!is.null(input$ongoing)) {
                print(input$ongoing)
                srqprep$prep_ongoing(
                    dat_lookback(),
                    .start = min(input$ongoing),
                    .end = max(input$ongoing),
                    .start_var = ordinerat,
                    .end_var = utsatt,
                    .new_name = "ongoing_timestamp"
                )
            } else {
                dat_lookback()
            }
        })

        # Rename to reflect the fact that data cleaning is finished
        out <- dat_ongoing

        n_cases <- sh$reactive({
            # check rows of data frame unless lan is NULL
            # hack necessary because sift_vars skips NULL inputs
            if (!is.null(input$lan)) {
                n <- sum(!is.na(out()[[.var %||% input$outcome]]))
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
