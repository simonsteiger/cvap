box::use(
  sh = shiny,
  bsl = bslib,
  dp = dplyr,
  tdr = tidyr,
  pr = purrr,
  rt = shiny.router,
  sass,
  magrittr[`%>%`],
)

box::use(
  nb = app / logic / navbox,
  app / logic / theme,
  app / view / menu,
  app / view / vap / vap_indikatorer_1,
  app / view / vap / vap_indikatorer_2,
  app / view / vap / vap_indikatorer_3,
  app / view / vap / vap_indikatorer_4,
  app / view / vap / vap_behandling_1,
  app / view / vap / vap_behandling_2,
  app / view / vap / vap_behandling_3,
  app / view / vap / vap_behandling_4,
  app / view / vap / vap_inklusionsmatt_1,
  app / view / vap / vap_kvalitetssakring_1,
  app / view / vap / vap_kvalitetssakring_2,
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
    rt$router_ui(
      rt$route("/", menu$ui(ns("menu"), nb$navbox_data)),
      rt$route("vap_indikatorer_1", vap_indikatorer_1$ui(ns("vap_indikatorer_1"))),
      rt$route("vap_indikatorer_2", vap_indikatorer_2$ui(ns("vap_indikatorer_2"))),
      rt$route("vap_indikatorer_3", vap_indikatorer_3$ui(ns("vap_indikatorer_3"))),
      rt$route("vap_indikatorer_4", vap_indikatorer_4$ui(ns("vap_indikatorer_4"))),
      rt$route("vap_behandling_1", vap_behandling_1$ui(ns("vap_behandling_1"))),
      rt$route("vap_behandling_2", vap_behandling_2$ui(ns("vap_behandling_2"))),
      rt$route("vap_behandling_3", vap_behandling_3$ui(ns("vap_behandling_3"))),
      rt$route("vap_behandling_4", vap_behandling_4$ui(ns("vap_behandling_4"))),
      rt$route("vap_inklusionsmatt_1", vap_inklusionsmatt_1$ui(ns("vap_inklusionsmatt_1"))),
      rt$route("vap_kvalitetssakring_1", vap_kvalitetssakring_1$ui(ns("vap_kvalitetssakring_1"))),
      rt$route("vap_kvalitetssakring_2", vap_kvalitetssakring_2$ui(ns("vap_kvalitetssakring_2"))),
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
    rt$router_server("/")

    menu$server("menu", nb$navbox_data)
    vap_indikatorer_1$server("vap_indikatorer_1")
    vap_indikatorer_2$server("vap_indikatorer_2")
    vap_indikatorer_3$server("vap_indikatorer_3")
    vap_indikatorer_4$server("vap_indikatorer_4")
    vap_behandling_1$server("vap_behandling_1")
    vap_behandling_2$server("vap_behandling_2")
    vap_behandling_3$server("vap_behandling_3")
    vap_behandling_4$server("vap_behandling_4")
    vap_inklusionsmatt_1$server("vap_inklusionsmatt_1")
    vap_kvalitetssakring_1$server("vap_kvalitetssakring_1")
    vap_kvalitetssakring_2$server("vap_kvalitetssakring_2")
  })
}
