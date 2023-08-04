box::use(
    magrittr[`%>%`],
    sh = shiny,
    rl = rlang,
    fct = forcats,
    dp = dplyr,
    ts = tidyselect,
    lub = lubridate,
)

box::use(
    app / logic / srqlib / srqprep,
    ase = app / logic / aux_server,
)

#' @export
ui <- function(id) {
    ns <- sh$NS(id)
    sh$tagList()
}

#' @export
server <- function(id, .data, group = NULL, ...) {
    sh$moduleServer(id, function(input, output, session) {
        dots <- rl$quos(...)

        sh$reactive({
            # First check if there's sortable data, if not, do nothing
            if (nrow(.data()) > 0 && all(is.na(.data()$outcome))) { # is the nrow check necessary?
                return(.data())
            } else if (input$sort) { # TRUE = alphabetical
                return(ase$sort_alph(.data()))
            } else if (is.null(group)) { # no custom reordering necessary, just order by outcome
                return(ase$sort_nogroup(.data()))
            } else if (is.factor(.data()[[group]])) {
                return(ase$sort_fct(.data(), group))
            } else if (lub$is.Date(.data()[[group]])) {
                return(ase$sort_date(.data(), group))
            } else if (is.numeric(.data()[[group]])) {
                return(ase$sort_num(.data(), group))
            }
        })
    })
}
