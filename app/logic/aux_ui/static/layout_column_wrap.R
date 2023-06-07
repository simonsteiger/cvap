box::use(
    bsl = bslib,
    ht = htmltools,
)

#' @export
layout_column_wrap <- function(..., grid_template_columns = "1fr 1fr") {
    bsl$layout_column_wrap(
        class = "my-3",
        width = NULL, height = 650,
        style = ht$css(grid_template_columns = grid_template_columns),
        ...
    )
}
