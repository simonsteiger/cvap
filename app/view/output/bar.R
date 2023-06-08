box::use(
    sh = shiny,
)

box::use(
    ase = app / logic / aux_server,
)

#' @export
ui <- function(id) {
    ns <- sh$NS(id)
    sh$tagList()
}

#' @export
server <- function(id, .data, x, y, group) {
    sh$moduleServer(id, function(input, output, session) {
        stopifnot(sh$is.reactive(.data))

        ase$plot_bar(
            .data,
            x = x,
            y = y,
            group = group
        )
    })
}
