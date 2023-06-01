box::use(
  sh = shiny,
  bsl = bslib,
  dp = dplyr,
  tdr = tidyr,
  pr = purrr,
  sass,
  magrittr[`%>%`],
)

box::use(
  nb = app / logic / navbox,
  app / logic / theme,
  app / view / menu,
)

#' @export
ui <- function(id) {
  ns <- sh$NS(id)
  bsl$page(
    theme = theme$light %>%
      bsl$bs_add_rules(sass$sass_file("app/styles/navbox.scss")),
    sh$div(
      class = "h1 d-flex justify-content-center",
      "Visualiserings- och analysplattform"
    ),
    menu$ui("menu", nb$navbox_data),
    sh$div(
      class = "h3 d-flex justify-content-center",
      "SRQ Logo"
    )
  )
}

#' @export
server <- function(id) {
  sh$moduleServer(id, function(input, output, session) {
    menu$server("menu", nb$navbox_data)
  })
}
