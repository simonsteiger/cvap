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

# Test with non reactive version
# st_sift_vars <- function(data, input, skip = NULL) {
#   vars <- colnames(data)
# 
#   each_var <-
#     pr$map(vars, \(v) sift_cols(data[[v]], input[[v]], v, skip))
# 
#   pr$reduce(each_var, `&`)
# }
# 
# out
# 
# input <- list(ordinerat = c(ymd("2020-01-01"), ymd("2021-01-01")), kon = "Kvinna")
# 
# x <- st_sift_vars(out, input)
# 
# out[x, ]
