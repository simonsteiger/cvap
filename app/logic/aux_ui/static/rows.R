box::use(
    sh = shiny,
    rl = rlang[`%||%`],
    pr = purrr,
)

#' @export
row <- function(left = NULL,
                center = NULL,
                right = NULL,
                class_row = NULL,
                class_col = NULL,
                colwidths = c(2, 8, 2)) {
    cls_cols <- paste(paste0("col-", colwidths), class_col, sep = " ")

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

#' @export
row2 <- function(content = list(), class = NULL, colwidths = list()) {
    stopifnot(length(content) == length(colwidths))
    stopifnot(is.numeric(unlist(colwidths)))
    stopifnot(sum(unlist(colwidths)) == 12) # TODO check if all number pairs are 12

    out <- pr$map(seq_along(content), \(i) {
        sh$div(
            class = paste0("col-", colwidths[[i]]),
            content[[i]]
        )
    })

    sh$div(
        class = class %||% "row m-4",
        out
    )
}

#' @export
row_sidebar <- function(sidebar, main) {
    sh$div(
        class = "row m-4",
        sh$div(
            class = "col-xxl-2 col-lg-3 col-md-4 col-sm-12",
            sidebar
        ),
        sh$div(
            class = "col-xxl-10 col-lg-9 col-md-8 col-sm-12",
            sh$div(
                class = "row",
                sh$div(
                    class = "col-xxl-7 col-lg-12 col-sm-12",
                    main[[1]]
                ),
                sh$div(
                    class = "col-xxl-5 col-lg-6 col-sm-12",
                    main[[2]]
                ),
                sh$div(
                    class = "col-xxl-5 col-lg-6 col-sm-12",
                    main[[3]]
                ),
                sh$div(
                    class = "col-xxl-7 col-lg-12 col-sm-12",
                    main[[4]]
                )
            )
        )
    )
}
