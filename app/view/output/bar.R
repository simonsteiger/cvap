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
server <- function(id, .data, x = "lan", y = "outcome", group = NULL, text = "Title", format = NULL) {
    sh$moduleServer(id, function(input, output, session) {
        stopifnot(sh$is.reactive(.data))

        ase$plot_bar(
            .data,
            x = x,
            y = y,
            group = group,
            text = text,
            format = format
        )
    })
}
