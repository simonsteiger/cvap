# The main module sets up the overarching app structure
# It reads data and creates UIs and servers for the 11 VAP pages

box::use(
  sh = shiny,
  bsl = bslib,
  dp = dplyr,
  tdr = tidyr,
  pr = purrr,
  rt = shiny.router,
  gj = geojsonio,
  gjsf = geojsonsf,
  sass,
  magrittr[`%>%`],
  sht = showtext,
  syf = sysfonts,
  shf = shinyFeedback,
  shj = shinyjs,
  waiter,
)

box::use(
  ski = app / logic / swissknife / skinit,
  app / logic / swissknife / sklang[`%//%`],
  aui = app / logic / aux_ui,
  app / logic / theme,
  app / logic / data / summaries,
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

# Read all data files in the data folder
ski$read_dir("app/logic/data/srq/clean/")

# Load the geo data
geo_json <- gj$geojson_read("app/logic/data/gadm/sweden.geojson")
geo_sf <- gjsf$geojson_sf(readLines("app/logic/data/gadm/sweden.geojson"))

# Add the fonts needed for pdf exports
syf$font_add_google("Roboto", "Roboto")
syf$font_add_google("Fraunces", "Fraunces")
syf$font_add_google("Commissioner", "Commissioner")
sht$showtext_auto()

# Get unique csDMARDs and bDMARDs for preparat info modal
info_dmard <- pr$set_names(c("csdmard", "bioprep")) %>%
  pr$map(\(type) {
    drugs <- list_df$vap_behandling_4 %>%
      dp$filter(prep_typ == type) %>%
      dp$pull(preparat) %>%
      unique()
    sh$div(
      sh$tags$h6(class = "text-center", type),
      sh$tags$ul(class = "small", pr$map(drugs, sh$tags$li))
    )
  }) %>%
  sh$div(class = "d-flex flex-row justify-content-evenly", .)

#' @export
ui <- function(id) {
  ns <- sh$NS(id)
  bsl$page(
    shj$useShinyjs(),
    shf$useShinyFeedback(),
    waiter$useWaiter(),
    waiter$waiterPreloader(
      html = sh$div(
        class = "fs-4 d-flex flex-column gap-3",
        waiter$spin_folding_cube(),
        "Initiera VAP, var god vänta..."
      ),
      fadeout = TRUE,
      color = "#4161ab"
    ),
    theme = theme$light %>%
      bsl$bs_add_rules(sass$sass_file("app/styles/navbox.scss")),
    rt$router_ui(
      rt$route(
        "/",
        home$ui(
          ns("home"),
          aui$navbox_data,
          info = list(icds = summaries$icd_list, dmard = info_dmard)
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
      geo = list(json = geo_json, sf = geo_sf),
      summary = list(
        text = summaries$indikatorer_1,
        extra_info = list(icds = summaries$icd_ra)
      )
    )

    vap_indikatorer_2$server(
      "vap_indikatorer_2",
      access_page = input$vap_indikatorer_2,
      data = list_df$vap_indikatorer_2,
      geo = list(json = geo_json, sf = geo_sf),
      summary = list(
        text = summaries$indikatorer_2,
        extra_info = list(icds = summaries$icd_ra)
      )
    )

    vap_indikatorer_3$server(
      "vap_indikatorer_3",
      access_page = input$vap_indikatorer_3,
      data = list_df$vap_indikatorer_3,
      geo = list(json = geo_json, sf = geo_sf),
      summary = list(
        text = summaries$indikatorer_3,
        extra_info = list(icds = summaries$icd_ra)
      )
    )

    vap_indikatorer_4$server(
      "vap_indikatorer_4",
      access_page = input$vap_indikatorer_4,
      data = list_df$vap_indikatorer_4,
      geo = list(json = geo_json, sf = geo_sf),
      summary = list(
        text = summaries$indikatorer_4,
        extra_info = list(icds = summaries$icd_ra)
      )
    )

    vap_behandling_1$server(
      "vap_behandling_1",
      access_page = input$vap_behandling_1,
      data = list_df$vap_behandling_1,
      geo = list(json = geo_json, sf = geo_sf),
      summary = list(
        text = summaries$behandling_1,
        extra_info = list(icds = summaries$icd_ra)
      )
    )

    vap_behandling_2$server(
      "vap_behandling_2",
      access_page = input$vap_behandling_2,
      data = list_df$vap_behandling_2,
      geo = list(json = geo_json, sf = geo_sf),
      summary = list(
        text = summaries$behandling_2,
        extra_info = list(icds = summaries$icd_ra)
      )
    )

    vap_behandling_3$server(
      "vap_behandling_3",
      access_page = input$vap_behandling_3,
      data = list_df$vap_behandling_3,
      geo = list(json = geo_json, sf = geo_sf),
      summary = list(
        text = summaries$behandling_3,
        extra_info = list(icds = summaries$icd_ra)
      )
    )

    vap_behandling_4$server(
      "vap_behandling_4",
      access_page = input$vap_behandling_4,
      data = list_df$vap_behandling_4,
      geo = list(json = geo_json, sf = geo_sf),
      summary = list(
        text = summaries$behandling_4,
        extra_info = list(icds = summaries$icd_ra)
      )
    )

    vap_inklusionsmatt_1$server(
      "vap_inklusionsmatt_1",
      access_page = input$vap_inklusionsmatt_1,
      data = list_df$vap_inklusionsmatt_1,
      geo = list(json = geo_json, sf = geo_sf),
      summary = list(
        text = summaries$inklusionsmatt_1,
        extra_info = list(icds = summaries$icd_ra)
      )
    )

    vap_kvalitetssakring_1$server(
      "vap_kvalitetssakring_1",
      access_page = input$vap_kvalitetssakring_1,
      data = list_df$vap_kvalitetssakring_1,
      geo = list(json = geo_json, sf = geo_sf),
      summary = list(
        text = summaries$kvalitetssakring_1,
        extra_info = list(icds = summaries$icd_ra)
      )
    )

    vap_kvalitetssakring_2$server(
      "vap_kvalitetssakring_2",
      access_page = input$vap_kvalitetssakring_2,
      data = list_df$vap_kvalitetssakring_2,
      geo = list(json = geo_json, sf = geo_sf),
      summary = list(
        text = summaries$kvalitetssakring_2,
        extra_info = list(icds = summaries$icd_ra)
      )
    )
  })
}
