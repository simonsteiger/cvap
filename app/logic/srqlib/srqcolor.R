box::use(
    cli,
    pal = palettes,
    rl = rlang[`%||%`],
)

#' @export
srqblu <- "#4161ab"

#' @export
pal_list <- pal$pal_palette(
    abyss = c(
        "#c6d9bd",
        "#9fd685",
        "#4f9e63",
        "#32575c"
    ),
    two = c(
        "#4161AB",
        "#71A774"
    ),
    three = c(
        "#4161AB",
        "#71A774",
        "#F8B859"
    ),
    four = c(
        "#CE5C30",
        "#F8B859",
        "#4161AB",
        "#71A774"
    ),
    five = c(
        "#923E88",
        "#CE5C30",
        "#F8B859",
        "#4161AB",
        "#71A774"
    ),
    six = c(
        "#923E88",
        "#98CAC9",
        "#CE5C30",
        "#F8B859",
        "#4161AB",
        "#71A774"
    ),
    seven = c(
        "#E681B1",
        "#923E88",
        "#98CAC9",
        "#CE5C30",
        "#F8B859",
        "#71A774",
        "#4161AB"
    )
)

#' @export
ramp <- function(n, palette = NULL) {
    if (is.null(palette)) {
        palette <- switch(as.character(n),
            "2" = "two",
            "3" = "three",
            "4" = "four",
            "5" = "five",
            "6" = "six",
            "7" = "seven",
            cli$cli_abort("No palette specified for {n} colors.")
        )
        anchor <- pal_list[[palette]]
    } else {
        anchor <- pal_list[[palette]] %||% palette
    }

    if (is.null(anchor)) {
        cli$cli_abort("Please provide a color palette.")
    }

    pal$pal_ramp(anchor, n = n, interpolate = "spline")
}

#' @export
reverse <- function(p) {
    stopifnot(length(p) > 0)
    new_p <- rep(0, length(p))

    for (i in seq_along(p)) {
        new_p[i] <- p[length(p) - i + 1]
    }
    new_p
}
