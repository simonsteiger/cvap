box::use(
    sh = shiny,
    bsl = bslib,
)

#' @export
ui <- function(id) {
    ns <- sh$NS(id)
    sh$tagList(
        "this is VAP behandling 1"
    )
}

#' @export
server <- function(id) {
    sh$moduleServer(id, function(input, output, session) {
        
    })
}