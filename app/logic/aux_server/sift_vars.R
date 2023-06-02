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

  vars <- sh$reactive(names(data()))

  sh$reactive({
    each_var <- pr$map(vars(), \(var) sift_cols(data()[[var]], input[[var]], var, skip))
    pr$reduce(each_var, `&`)
  })
}