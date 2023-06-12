box::use(
    sh = shiny,
    rl = rlang[`%||%`],
)

#' @export
row <- function(left = NULL,
                center = NULL,
                right = NULL,
                class_row = NULL,
                class_col = NULL,
                colwidths = c(1, 10, 1)) {
    cls_cols <- paste(paste0("col-", colwidths), class_col, sep = " ")
    cls_row <- paste("row", class_row, sep = " ")

    sh$div(
        class = class_row %||% "row m-4",
        sh$div(
            class = cls_cols[1],
            left,
        ),
        sh$div(
            class = cls_cols[2],
            center,
        ),
        sh$div(
            class = cls_cols[3],
            right,
        )
    )
}
