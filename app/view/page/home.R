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
                colwidths = c(2, 8, 2),
                center = sh$div(
                    class = "d-flex flex-column justify-content-center align-items-center gap-3",
                    sh$tags$img(class = "pt-3", src = "static/SRQ_langlogga.png", width = "420px"),
                    sh$h1(class = "fs-1 h-font", "Visualiserings- och Analysplattform")
                )
            ),
            aui$row(
                colwidths = c(0, 12, 0),
                center = sh$div(
                    class = "d-flex flex-wrap align-items-stretch justify-content-center",
                    !!!aui$navbox_map(id, data)
                )
            ) # ,
            # aui$row(
            #    colwidths = c(2, 8, 2),
            #    center = sh$div(
            #        class = "h3 d-flex justify-content-center",
            #        sh$tags$img(src = "static/SRQ_langlogga.png", width = "40%")
            #    )
            # )
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
