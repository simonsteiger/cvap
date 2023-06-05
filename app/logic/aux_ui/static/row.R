box::use(
    sh = shiny,
)

#' @export
row <- function(left = NULL, center = NULL, right = NULL, colwidths = c(2, 8, 2)) {
    cols <- paste0("col-", colwidths)
    sh$div(
        class = "row",
        sh$div(
            class = cols[1],
            left,
        ),
        sh$div(
            class = cols[2],
            center,
        ),
        sh$div(
            class = cols[3],
            right,
        )
    )
}
