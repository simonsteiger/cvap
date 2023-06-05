box::use(
    sh = shiny,
)

box::use(
    ase = app / logic / aux_server
)

#' @export
ui <- function(id, ...) {
    ns <- sh$NS(id)
    sh$tagList(...)
}

#' @export
server <- function(id, data, input) {
    sh$moduleServer(id, function(input, output, session) {
        ase$sift_vars(data, input)
    })
}
