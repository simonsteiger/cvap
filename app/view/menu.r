box::use(
    sh = shiny,
    pr = purrr,
    tdr = tidyr,
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

        change_page <- function(name, tag) {
            sh$observeEvent(input[[paste("vap", name, tag, sep = "-")]], {
                sh$updateTabsetPanel(session, "menu", selected = paste("vap", name, tag, sep = "-"))
            })
        }

        pr$pmap(tdr$unnest_longer(data, tag), change_page)
    })
}
