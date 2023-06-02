box::use(
    sh = shiny,
    pr = purrr,
)

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