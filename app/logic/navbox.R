box::use(
  dp = dplyr,
  sh = shiny,
  pr = purrr,
  tbl = tibble,
)

#' @export
navbox_map <- function(data) {
  pr$map(
    seq_len(nrow(data)),
    ~ navbox(data[.x, ])
  )
}

#' @export
navbox <- function(data) {
  sh$div(
    class = "recipe",
    sh$h3(class = "name", data$name),
    sh$div(
      class = "d-flex flex-wrap",
      pr$map(
        pr$list_flatten(data$tags),
        ~ sh$div(class = "tags", .x)
      )
    )
  )
}

#' @export
navbox_data <- tbl$tibble(
  name = c(
    "Indikatorer",
    "Behandling",
    "Inklusionsmått",
    "Kvalitetssäkring"
  ),
  tags = list(
    list(
      "Sjukdomsduration vid nydiagnosticerad RA",
      "Befolkningsjämförelse av antal personer med RA och biologiska läkemedel",
      "Patientrapporterad hälsa vid behandling med biologiska läkemedel för RA",
      "Sjukdomsaktivitet efter start av första biologiska läkemedel för RA"
    ),
    list(
      "Pågående DMARD",
      "Befolkningsjämförelse av antal personer med biologiska läkemedel",
      "Låg sjukdomsaktivitet vid RA och pågående biologiska läkemedel",
      "Patientrapporterad hälsa vid behandling"
    ),
    list("Sjukdomsduration vid diagnos"),
    list(
      "Uppföljningsbesök efter start av läkemedelsbehandling",
      "Täckningsgrad för RA"
    )
  )
)
