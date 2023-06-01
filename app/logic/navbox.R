box::use(
  dp = dplyr,
  sh = shiny,
  pr = purrr,
  tbl = tibble,
)

#' @export
navbox_map <- function(id, data) {
  stopifnot(all(c("title", "url", "tag") %in% colnames(data)))

  pr$map(
    seq_len(nrow(data)),
    ~ navbox(id, data[.x, ])
  )
}

#' @export
navbox <- function(id, data) {
  sh$div(
    class = "navbox card",
    sh$h3(class = "title", data$title),
    sh$div(
      class = "d-flex flex-wrap",
      pr$map2(
        seq_along(pr$list_flatten(data$tag)),
        pr$list_flatten(data$tag),
        \(num, tag) sh$actionButton(
          class = "tag",
          sh$NS(id, paste("vap", tolower(data$url), num, sep = "_")), tag
        )
      )
    )
  )
}

#' @export
navbox_data <- tbl$tibble(
  title = c(
    "Indikatorer",
    "Behandling",
    "Inklusionsmått",
    "Kvalitetssäkring"
  ),
  url = c(
    "Indikatorer",
    "Behandling",
    "Inklusionsmatt",
    "Kvalitetssakring"
  ),
  tag = list(
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
