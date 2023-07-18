box::use(
    sh = shiny,
    pr = purrr,
    shw = shinyWidgets,
    shf = shinyFeedback,
    magrittr[`%>%`],
)

box::use(
    ase = app / logic / aux_server
)

#' @export
ui <- function(id) {
    ns <- sh$NS(id)
    sh$tagList(
        sh$textOutput(ns("no_lan")) # output of warning when no lan selected
    )
}

#' @export
server <- function(id, data) {
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

        no_lan <- sh$reactive(
            shf$feedbackDanger(
                "lan",
                is.null(input$lan),
                "Välj minst ett län.",
                session = session
            )
        )

        output$no_lan <- sh$renderText(no_lan())

        ase$sift_vars(data, input)
    })
}
