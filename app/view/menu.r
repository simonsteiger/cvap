box::use(
    sh = shiny,
    pr = purrr,
    rt = shiny.router,
)

box::use(
    nb = app / logic / navbox,
)

#' @export
ui <- function(id, data) {
    sh$div(
        class = "d-flex flex-wrap align-items-stretch justify-content-center",
        !!!nb$navbox_map(id, data)
    )
}

#' @export
server <- function(id, data) {
    sh$moduleServer(id, function(input, output, session) {
        stopifnot("tag" %in% colnames(data))

        wrap_change_page <- function(data) {
            pr$map(seq_along(pr$list_flatten(data$tag)), \(t) {
                input_id <- paste("vap", tolower(data$url), t, sep = "_")
                sh$observeEvent(input[[input_id]], {
                    rt$change_page(input_id)
                })
            })
        }

        pr$map(seq_len(nrow(data)), \(r) wrap_change_page(data[r, ]))
    })
}
