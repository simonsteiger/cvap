box::use(
    sh = shiny,
    pr = purrr,
    shw = shinyWidgets,
    shf = shinyFeedback,
    dp = dplyr,
    lub = lubridate,
    magrittr[`%>%`],
    rl = rlang[`%||%`],
)

box::use(
    ase = app / logic / aux_server,
    swissknife / sklang[`%//%`],
)

vali_date <- function(input) {
    options <- c("inkluderad", "ordinerat", "ongoing")

    present_date <- sh$reactive(
        options[options %in% names(input)]
    )

    cnd <- all(pr$map(input[[present_date]], lub$is.Date))

    sh$req(cnd, cancelOutput = TRUE)

    shf$feedbackDanger(
        present_date,
        cnd,
        "Ogiltig datum."
    )
}


#' @export
ui <- function(id) {
    ns <- sh$NS(id)
    sh$tagList(
        sh$textOutput(ns("no_lan")), # output of warning when no lan selected
        sh$textOutput(ns("no_date")), # output of warning when no lan selected
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

        # Send a feedback to user if no läns selected
        no_lan <- sh$reactive(
            shf$feedbackDanger(
                "lan",
                is.null(input$lan),
                "Välj minst ett län.",
                session = session
            )
        )

        # Filtering if date input is OK
        out <- sh$reactive({
            options <- c("inkluderad", "ordinerat", "ongoing")

            present_date <- options[options %in% names(input)]

            if (length(present_date) > 0) { # present_date can't be char(0)
                print(present_date)
                cnd <- all(pr$map_lgl(input[[present_date]], lub$is.Date))

                no_lan <- shf$feedbackDanger(
                    present_date,
                    !cnd,
                    "Ogiltig datum."
                )

                sh$req(cnd, cancelOutput = TRUE)
            }

            sieve <- ase$sift_vars(data, input)

            data()[sieve(), ] %>%
                ase$maybe_lookback(input, .var) %>%
                ase$maybe_ongoing(input)
        })

        # check rows of data frame unless lan is NULL (NULL means no filter in sift_vars)
        # if not NULL, count
        # else, 0
        n_cases <- sh$reactive({
            if (!is.null(input$lan)) {
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

        output$no_text <- sh$renderText(no_text())

        output$n_cases <- sh$renderUI(n_cases())

        out
    })
}
