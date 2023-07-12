box::use(
  bsl = bslib,
  gg = ggplot2,
  ggt = ggtext,
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
    bsl$font_google("Roboto Flex", local = FALSE), "Roboto", "serif"
  ),
  heading_font = bsl$font_google("Commissioner"),
  code_font = bsl$font_google("Roboto Mono")
)

#' @export
ggexport <- gg$theme(
  text = gg$element_text(family = "Roboto"),
  legend.title = gg$element_blank(),
  legend.position = "bottom",
  plot.title = ggt$element_textbox_simple(
    family = "Fraunces",
    # colour = srqcolor$srqblu,
    hjust = 0,
    size = 18,
    # margin = gg$margin(t = 10) # handle all outer margins in plot.margin
  ),
  plot.subtitle = ggt$element_textbox_simple(
    family = "Roboto",
    # colour = srqcolor$srqblu,
    hjust = 0,
    lineheight = 1.2,
    margin = gg$margin(t = 20, b = 20)
  ),
  plot.caption = gg$element_text(
    family = "Roboto",
    # colour = srqcolor$srqblu,
    hjust = 1,
    lineheight = 1,
    margin = gg$margin(t = 20)
  ),
  plot.margin = gg$margin(25, 30, 25, 25),
  plot.background = gg$element_blank(),
  strip.background = gg$element_blank(),
  strip.placement = "outside"
)
