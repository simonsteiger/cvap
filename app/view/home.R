box::use(
    sh = shiny,
    pr = purrr,
    rt = shiny.router,
)

box::use(
    aui = app / logic / aux_ui,
)

#' @export
ui <- function(id, data) {
    sh$tagList(
        aui$container_fluid(
            aui$row(
                center = aui$head()
            ),
            aui$row(
                center = sh$div(
                    class = "d-flex flex-wrap align-items-stretch justify-content-center mx-5",
                    !!!aui$navbox_map(id, data)
                )
            ),
            aui$row(
                center = aui$logo()
            )
        )
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
