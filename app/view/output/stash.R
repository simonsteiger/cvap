box::use(
    sh = shiny,
    gg = ggplot2,
    gl = glue,
    dp = dplyr,
    pr = purrr,
    rl = rlang[`%||%`],
)

box::use(
    ase = app / logic / aux_server,
)

#' @export
#' Stash saves user inputs for renaming plots and tables later
#' This is necessary because inputs are in "input" namespace
#' and can't simply be accessed from "output" namespace (where plot and table are)
server <- function(id, title, .var = NULL) {
    sh$moduleServer(id, function(input, output, session) {
        subtitle <- sh$reactive({
            ase$create_subtitle(input, .var)
        })

        sh$reactive({
            list(
                title = title,
                subtitle = subtitle(),
                outcome = input$outcome %||% .var,
                input = input
            )
        })
    })
}
