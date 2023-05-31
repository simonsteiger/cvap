box::use(
  sh = shiny,
  bsl = bslib,
  dp = dplyr,
  sass,
  magrittr[`%>%`],
)

box::use(
  nb = app / logic / navbox,
  theme = app / logic / theme,
)

#' @export
ui <- function(id) {
  ns <- sh$NS(id)
  bsl$page(
    theme = theme$light %>%
      bsl$bs_add_rules(sass$sass_file("app/styles/navbox.scss")),
    sh$div(
      class = "h1 d-flex justify-content-center",
      "Visualisation and Analysis Platform"
    ),
    sh$div(
      class = "d-flex flex-wrap align-items-stretch justify-content-center",
      !!!nb$navbox_map(nb$navbox_data)
    ),
    sh$div(
      class = "h3 d-flex justify-content-center",
      "SRQ Logo"
    )
  )
}

#' @export
server <- function(id) {
  sh$moduleServer(id, function(input, output, session) {

  })
}
