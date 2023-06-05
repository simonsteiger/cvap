box::use(
    pr = purrr,
)

box::use(
    app / logic / aux_ui / static / navbox[...]
)

#' @export
navbox_map <- function(id, data) {
  stopifnot(all(c("title", "url", "tag") %in% colnames(data)))

  pr$map(
    seq_len(nrow(data)),
    ~ navbox(id, data[.x, ])
  )
}
