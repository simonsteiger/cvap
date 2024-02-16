box::use(
    sh = shiny,
    pr = purrr,
    rt = shiny.router,
    bsl = bslib,
    bsi = bsicons,
    shj = shinyjs,
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
                colwidths = list(1, 10, 1),
                content = list(
                    NULL,
                    sh$div(
                        class = "d-flex flex-column justify-content-center align-items-center gap-3",
                        sh$tags$img(src = "static/logo_wide.png", width = "420px"),
                        sh$div(class = "fs-1 h-font text-center", "Visualiserings- och Analysplattform")
                    ),
                    sh$tags$a(
                        href = "https://www.github.com/simonsteiger/cvap",
                        class = "btn btn-secondary btn-white hover",
                        target = "_blank",
                        sh$div(
                            class = "d-flex flex-row align-items-center gap-2 small",
                            sh$icon(class = "fs-4", "github"),
                            "Källkod"
                        )
                    )
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
                class = "row m-4 pt-2",
                colwidths = list(6, 6),
                content = list(
                    sh$div(
                        class = "d-flex flex-row align-items-center justify-content-end",
                        aui$btn_modal(
                            id = ns("modal_icd"),
                            label = sh$div(
                                class = "d-flex flex-row small align-items-center",
                                "Om preparatgrupper"
                            ),
                            modal_title = "Information om preparatgrupper",
                            footer_confirm = NULL,
                            footer_dismiss = "Tillbaka",
                            class_toggle = "btn btn-secondary btn-white hover small",
                            info$dmard
                        )
                    ),
                    sh$div(
                        class = "d-flex flex-row align-items-center justify-content-start",
                        aui$btn_modal(
                            id = ns("modal_dmard"),
                            label = sh$div(
                                class = "d-flex flex-row small align-items-center",
                                "Om diagnosgrupper"
                            ),
                            modal_title = "Information om diagnoser",
                            footer_confirm = NULL,
                            footer_dismiss = "Tillbaka",
                            class_toggle = "btn btn-secondary btn-white hover small",
                            pr$map(info$icds, ase$icd_compose)
                        )
                    )
                )
            ),
            aui$row2(
                class = "row m-4 pt-2",
                colwidths = list(4, 4, 4),
                content = list(
                    NULL,
                    sh$div(
                        class = "d-flex flex-row align-items-center justify-content-center",
                        sh$div(
                            class = "d-flex flex-row align-items-center justify-content-center gap-2",
                            style = "background-color: #f8b859; padding: 0.5rem; border-radius: 1rem;",
                            sh$icon("screwdriver-wrench"),
                            "Utvecklingen pågår fortfarande på några VAPs.",
                            sh$icon("screwdriver-wrench")
                        )
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

        # Disable those VAPs that have errors
        shj$disable("vap_indikatorer_2")
        shj$disable("vap_behandling_2")
        shj$disable("vap_behandling_3")

        pr$map(seq_len(nrow(data)), \(r) wrap_change_page(data[r, ]))
    })
}


# class = "d-flex justify-content-end align-items-center",
#                         aui$btn_modal(
#                             id = ns("modal_icd"),
#                             label = sh$div(
#                                 class = "d-flex flex-row small align-items-center",
#                                 "Vilka diagnoser ingår i vilken diagnosgrupp?"
#                             ),
#                             modal_title = "Information om diagnoser",
#                             footer_confirm = NULL,
#                             footer_dismiss = "Tillbaka",
#                             ase$icd_compose(info$icds)
#                         )
# class = "d-flex justify-content-start align-items-center",
#                         aui$btn_modal(
#                             id = ns("modal_drugs"),
#                             label = sh$div(
#                                 class = "d-flex flex-row small align-items-center",
#                                 "Vilka preparater ingår i bDMARD och csDMARD?"
#                             ),
#                             modal_title = "Information om preparater",
#                             footer_confirm = NULL,
#                             footer_dismiss = "Tillbaka",
#                             "Nothing here yet"
#                         )
