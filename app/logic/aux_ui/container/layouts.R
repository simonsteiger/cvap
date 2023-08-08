box::use(
    sh = shiny,
    rl = rlang[`%||%`],
    pr = purrr,
    ht = htmltools,
)

box::use(
    app / logic / aux_ui / input / buttons[btn_return],
)

#' @export
container_fluid <- function(...) {
    sh$div(
        class = "container-fluid",
        ...
    )
}

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
    stopifnot(sum(unlist(colwidths)) == 12)

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

#' @export
head <- function(id, title) {
    row2(
        class = "row py-4 m-4 d-flex justify-content-center align-items-center",
        colwidths = list(2, 8, 2),
        content = list(
            sh$div(btn_return(id)),
            sh$div(class = "fs-1 h-font text-center", title),
            sh$div(
                class = "justify-content-end",
                sh$img(src = "static/logo_wide.png", width = "100%")
            )
        )
    )
}

#' @export
layout_column_wrap <- function(..., grid_template_columns = "1fr 1fr") {
    bsl$layout_column_wrap(
        class = "my-3",
        width = NULL, height = 650,
        style = ht$css(grid_template_columns = grid_template_columns),
        ...
    )
}
