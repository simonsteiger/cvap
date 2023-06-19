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
server <- function(id, .data, geo, x = "lan", y = "outcome", group = NULL, text = "Title") {
    sh$moduleServer(id, function(input, output, session) {
        stopifnot(sh$is.reactive(.data))

        ase$plot_map(
            .data,
            geo = geo,
            x = x,
            y = y,
            group = group,
            text = text
        )
    })
}

#' @export
wrap <- function(args) {
    server(
        id = args$id,
        .data = args$.data,
        geo = args$geo,
        x = args$x,
        y = args$y,
        group = args$group,
        text = args$text
    )
}
