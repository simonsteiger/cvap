box::use(
    dp = dplyr,
    cli,
    sc = scales,
    magrittr[`%>%`],
)

#' @export
guess_label_num <- function(axis, v, .scale = NULL, quiet = FALSE) {
  if (!is.null(.scale)) {
    return(.scale)
  }
  if (all(v %>% dp$between(., 0, 1), na.rm = TRUE)) {
    if (!quiet) {
      cli$cli_rule(right = "select_scales")
      cli$cli_bullets(c(
        " " = "Autoselect {.emph scale}",
        " " = "All values on {axis} axis between 0 and 1.",
        ">" = "Guessing {axis} axis shows percentages."
      ))
    }
    sc$label_percent()
  } else {
    sc$label_number()
  }
}
