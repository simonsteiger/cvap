box::use(
    fst,
    dp = dplyr,
    gg = ggplot2,
)

# Make sure to create fst with respective .R file before running this
data <- fst$read_fst("app/logic/data/vap_behandling_4.fst")

#' @export
ui <- function(id) {
    ns <- sh$NS(id)
    sh$tagList(
        # user inputs like ordering
    )
}

#' @export
server <- function(id, data) { # pass the data of the current vap
    sh$moduleServer(id, function(input, output, session) {
        # rct_summarise
    })
}