box::use(
    sh = shiny,
    pr = purrr,
    rt = shiny.router,
    bsl = bslib,
    bsi = bsicons,
)

box::use(
    aui = app / logic / aux_ui,
    ase = app / logic / aux_server,
)

#' @export
ui <- function(id, data, info) {
    ns <- sh$NS(id)
    sh$tagList(
        aui$container_fluid(
            aui$row2(
                colwidths = list(0, 12, 0),
                content = list(
                    NULL,
                    sh$div(
                        class = "d-flex flex-column justify-content-center align-items-center gap-3",
                        sh$tags$img(src = "static/logo_wide.png", width = "420px"),
                        sh$div(class = "fs-1 h-font text-center", "Visualiserings- och Analysplattform")
                    ),
                    NULL
                )
            ),
            aui$row2(
                colwidths = c(0, 12, 0),
                content = list(
                    NULL,
                    sh$div(
                        class = "d-flex flex-wrap align-items-stretch justify-content-center",
                        !!!aui$navbox_map(id, data)
                    ),
                    NULL
                )
            ),
            aui$row2(
                colwidths = list(3, 6, 3),
                content = list(
                    NULL,
                    sh$div(
                        class = "m-2 d-flex flex-row justify-content-center gap-4 align-items-center",
                        aui$btn_modal(
                            id = ns("summary_modal"),
                            label = sh$div(class = "d-flex flex-row gap-1 align-items-center", sh$icon(class = "fs-2", "book-medical"), "ICD-10 kods"),
                            modal_title = "Information om diagnoser",
                            footer_confirm = NULL,
                            footer_dismiss = "Tillbaka",
                            ase$icd_compose(info$icds)
                        ),
                        sh$tags$a(
                            href = "https://www.github.com/simonsteiger/cvap",
                            class = "btn btn-secondary hover",
                            target = "_blank",
                            sh$div(
                                class = "d-flex flex-row align-items-center gap-3",
                                sh$icon(class = "fs-2", "github"),
                                "Se källkod"
                            )
                        ) # ,
                        # sh$tags$button(
                        #    type = "button",
                        #    style = "pointer-events: none;",
                        #    class = "btn btn-transparent",
                        #    sh$div(
                        #        class = "d-flex flex-row align-items-center gap-3",
                        #        sh$icon(class = "fs-3 c-success", "arrow-up"),
                        #        "Patienter + 10 354"
                        #    )
                        # ),
                        # sh$tags$button(
                        #    type = "button",
                        #    style = "pointer-events: none;",
                        #    class = "btn btn-transparent",
                        #    sh$div(
                        #        class = "d-flex flex-row align-items-center gap-3",
                        #        sh$icon(class = "fs-3 c-danger", "arrow-down"),
                        #        "Besök -100 549"
                        #    )
                        # )
                    ),
                    NULL
                )
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
