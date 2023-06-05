box::use(
    tbl = tibble,
)

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
