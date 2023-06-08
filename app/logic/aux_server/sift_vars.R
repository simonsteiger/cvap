box::use(
  magrittr[`%>%`],
  sh = shiny,
  dp = dplyr,
  pr = purrr,
)

box::use(
  app / logic / aux_server / sift_cols[...],
)

#' @export
sift_vars <- function(data, input, skip = NULL) {
  stopifnot(sh$is.reactive(data))

  vars <- sh$reactive(colnames(data()))

  sh$reactive({
    each_var <-
      pr$map(vars(), \(v) sift_cols(data()[[v]], input[[v]], v, skip))

    pr$reduce(each_var, `&`)
  })
}
