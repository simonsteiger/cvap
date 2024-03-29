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
server <- function(id, title, .var = NULL, datecompare = FALSE) {
    sh$moduleServer(id, function(input, output, session) {
        subtitle <- sh$reactive({
            ase$create_subtitle(input, .var, datecompare)
        })

        title_suffix <- sh$reactive({
            ase$create_title_suffix(input, title)
        })

        sh$reactive({
            list(
                title = title_suffix(),
                subtitle = subtitle(),
                outcome = input$outcome %||% .var, # either input dependent or manually specified
                input = input
            )
        })
    })
}
