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
  warning = "#c26130",
  danger = "#fd5448",
  base_font = bsl$font_google("Fira Sans"),
  heading_font = bsl$font_google("Fira Sans"),
  code_font = bsl$font_google("Fira Sans")
)