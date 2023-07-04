box::use(
  sh = shiny,
  bsl = bslib,
  dp = dplyr,
  tdr = tidyr,
  pr = purrr,
  rt = shiny.router,
  gj = geojsonio,
  sass,
  magrittr[`%>%`],
)

box::use(
  ski = swissknife / skinit,
  aui = app / logic / aux_ui,
  app / logic / theme,
  app / view / page / home,
  app / view / page / vap_indikatorer_1,
  app / view / page / vap_indikatorer_2,
  app / view / page / vap_indikatorer_3,
  app / view / page / vap_indikatorer_4,
  app / view / page / vap_behandling_1,
  app / view / page / vap_behandling_2,
  app / view / page / vap_behandling_3,
  app / view / page / vap_behandling_4,
  app / view / page / vap_inklusionsmatt_1,
  app / view / page / vap_kvalitetssakring_1,
  app / view / page / vap_kvalitetssakring_2,
)

ski$read_dir("app/logic/data/")

geo <- gj$geojson_read("app/logic/data/gadm/sweden.geojson")

#' @export
ui <- function(id) {
  ns <- sh$NS(id)
  bsl$page(
    theme = theme$light %>%
      bsl$bs_add_rules(sass$sass_file("app/styles/navbox.scss")),
    rt$router_ui(
      rt$route(
        "/",
        home$ui(
          ns("home"),
          aui$navbox_data
        )
      ),
      rt$route(
        "vap_indikatorer_1",
        vap_indikatorer_1$ui(
          ns("vap_indikatorer_1"),
          list_df$vap_indikatorer_1
        )
      ),
      rt$route(
        "vap_indikatorer_2",
        vap_indikatorer_2$ui(
          ns("vap_indikatorer_2"),
          list_df$vap_indikatorer_2
        )
      ),
      rt$route(
        "vap_indikatorer_3",
        vap_indikatorer_3$ui(
          ns("vap_indikatorer_3"),
          list_df$vap_indikatorer_3
        )
      ),
      rt$route(
        "vap_indikatorer_4",
        vap_indikatorer_4$ui(
          ns("vap_indikatorer_4"),
          list_df$vap_indikatorer_4
        )
      ),
      rt$route(
        "vap_behandling_1",
        vap_behandling_1$ui(
          ns("vap_behandling_1"),
          list_df$vap_behandling_1
        )
      ),
      # How do behandling 2 and indikatorer 2 differ (meaningfully)?
      rt$route(
        "vap_behandling_2",
        vap_behandling_2$ui(
          ns("vap_behandling_2"),
          list_df$vap_behandling_2
        )
      ),
      rt$route(
        "vap_behandling_3",
        vap_behandling_3$ui(
          ns("vap_behandling_3"),
          list_df$vap_behandling_3
        )
      ),
      rt$route(
        "vap_behandling_4",
        vap_behandling_4$ui(
          ns("vap_behandling_4"),
          list_df$vap_behandling_4
        )
      ),
      rt$route(
        "vap_inklusionsmatt_1",
        vap_inklusionsmatt_1$ui(
          ns("vap_inklusionsmatt_1"),
          list_df$vap_inklusionsmatt_1
        )
      ),
      rt$route(
        "vap_kvalitetssakring_1",
        vap_kvalitetssakring_1$ui(
          ns("vap_kvalitetssakring_1"),
          list_df$vap_kvalitetssakring_1
        )
      ),
      rt$route(
        "vap_kvalitetssakring_2",
        vap_kvalitetssakring_2$ui(
          ns("vap_kvalitetssakring_2"),
          list_df$vap_kvalitetssakring_2
        )
      ),
    )
  )
}

#' @export
server <- function(id) {
  sh$moduleServer(id, function(input, output, session) {
    rt$router_server("/")

    home$server("home", aui$navbox_data)

    vap_indikatorer_1$server(
      "vap_indikatorer_1",
      access_page = input$vap_indikatorer_1,
      data = list_df$vap_indikatorer_1,
      geo = geo
    )

    vap_indikatorer_2$server(
      "vap_indikatorer_2",
      access_page = input$vap_indikatorer_2,
      data = list_df$vap_indikatorer_2,
      geo = geo
    )

    vap_indikatorer_3$server(
      "vap_indikatorer_3",
      access_page = input$vap_indikatorer_3,
      data = list_df$vap_indikatorer_3,
      geo = geo
    )

    vap_indikatorer_4$server(
      "vap_indikatorer_4",
      access_page = input$vap_indikatorer_4,
      data = list_df$vap_indikatorer_4,
      geo = geo
    )

    vap_behandling_1$server(
      "vap_behandling_1",
      access_page = input$vap_behandling_1,
      data = list_df$vap_behandling_1,
      geo = geo
    )

    vap_behandling_2$server(
      "vap_behandling_2",
      access_page = input$vap_behandling_2,
      data = list_df$vap_behandling_2,
      geo = geo
    )

    vap_behandling_3$server(
      "vap_behandling_3",
      access_page = input$vap_behandling_3,
      data = list_df$vap_behandling_3,
      geo = geo
    )

    vap_behandling_4$server(
      "vap_behandling_4",
      access_page = input$vap_behandling_4,
      data = list_df$vap_behandling_4,
      geo = geo
    )

    vap_inklusionsmatt_1$server(
      "vap_inklusionsmatt_1",
      access_page = input$vap_inklusionsmatt_1,
      data = list_df$vap_inklusionsmatt_1,
      geo = geo
    )

    vap_kvalitetssakring_1$server(
      "vap_kvalitetssakring_1",
      access_page = input$vap_kvalitetssakring_1,
      data = list_df$vap_kvalitetssakring_1,
      geo = geo
    )

    vap_kvalitetssakring_2$server(
      "vap_kvalitetssakring_2",
      access_page = input$vap_kvalitetssakring_2,
      data = list_df$vap_kvalitetssakring_2,
      geo = geo
    )
  })
}
