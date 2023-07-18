box::use(
    magrittr[`%>%`],
    dp = dplyr,
    sh = shiny,
    rl = rlang[`%||%`],
)

box::use(
    aui = app / logic / aux_ui,
)

icon_samplesize <- function(id, input, value) {
    sh$div(
        class = "d-flex flex-row justify-content-between align-items-center",
        sh$div(
            class = "d-flex flex-row align-items-center gap-2",
            sh$tags$i(class = "fa fa-users-slash icon-danger"),
            paste0("Få data i ", length(input), " län")
        ),
        aui$inp_toggle(id = id, label = "Dölj", value = value)
    )
}

icon_samplesize_modal <- function(id, input) {
    if (length(input) == 0) {
        return("Inga varningar")
    } else {
        aui$btn_modal(
            id = id,
            class_toggle = "btn btn-secondary hover",
            label = sh$div(
                class = "d-flex flex-row align-items-center gap-2",
                sh$tags$i(class = "fa fa-triangle-exclamation icon-danger"),
                paste0("Få data i ", length(input), " län")
            ),
            modal_title = "Varningar",
            footer_confirm = NULL,
            footer_dismiss = NULL,
            sh$div(
                paste0("Mindre än 10 observationer: ", paste0(input, collapse = ", "))
            )
        )
    }
}

#' @export
ui <- function(id) {
    ns <- sh$NS(id)
    sh$tagList(sh$htmlOutput(ns("sidebar")))
}

#' @export
server <- function(id, .data) {
    sh$moduleServer(id, function(input, output, session) {
        stopifnot(sh$is.reactive(.data))

        low_n_lans <- sh$reactive({
            sh$req(nrow(.data()) > 0)

            .data() %>%
                dp$filter(nonmissing < 10) %>%
                dp$pull(lan) %>%
                unique()
        })

        last_input <- sh$eventReactive(low_n_lans(), {
            input$exclude_low_n %||% FALSE
        })

        icons <- sh$reactive(
            icon_samplesize(
                session$ns("exclude_low_n"),
                low_n_lans(),
                last_input()
            )
        )

        output$sidebar <- sh$renderUI({
            if (length(low_n_lans()) >= 1) {
                aui$sidebar(
                    header = sh$div(class = "py-card-header", "Varningar"),
                    body = icons()
                )
            } else {
                NULL
            }
        })

        sh$reactive({
            if (input$exclude_low_n %||% FALSE) {
                .data() %>%
                    dp$filter(!lan %in% low_n_lans())
            } else {
                .data()
            }
        })
    })
}
