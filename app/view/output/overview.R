box::use(
    sh = shiny,
    dp = dplyr,
    bsl = bslib,
    pr = purrr,
    rl = rlang,
)

box::use(
    aui = app / logic / aux_ui,
    ase = app / logic / aux_server,
)



#' @export
ui <- function(id) {
    ns <- sh$NS(id)
    sh$tagList()
}

#' @export
server <- function(id) {
    sh$moduleServer(id, function(input, output, session) {
        inputs_with_icons <- names(input)[names(input) %in% names(ase$iconostasis)]

        # Iterate over all input names that have a corresponding icon-function
        # If current name is last name, make icon only
        # Else, make icon and hr tag
        pr$map(
            inputs_with_icons,
            \(name) {
                if (which(names(input) == name) == length(inputs_with_icons)) {
                    ase$iconostasis[[name]](input[[name]], id = id)
                } else {
                    sh$tagList(ase$iconostasis[[name]](input[[name]]), sh$hr())
                }
            }
        )
    })
}
