box::use(
  bsl = bslib,
)

#' @export
light <- bsl$bs_theme(
  version = 5,
  bg = "#ffffff",
  fg = "#4161ab",
  primary = "#f8b859",
  secondary = "#e8e9f3",
  success = "#a8e069",
  info = "#e8e9f3",
  warning = "#fbdbdb",
  danger = "#c9281d",
  base_font = bsl$font_collection(
    bsl$font_google("Roboto Flex", local = FALSE), "Roboto", "sans-serif"
  ),
  heading_font = bsl$font_google("Roboto Slab"),
  code_font = bsl$font_google("Roboto Mono")
)
