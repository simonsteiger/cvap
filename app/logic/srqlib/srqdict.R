box::use(
    rl = rlang,
)

#' @export
fil_ongoing <- function(at) {
    rl$quo(
        ordinerat <= at & (pagaende == 1 | utsatt > at)
    )
}

#' @export
rec_dxcat <- rl$exprs(
    .data[["diagnoskod_1"]] %in% c(
        "M05.3", "M05.8L", "M05.8M", "M05.8N",
        "M05.9", "M05.9L", "M05.9M", "M05.9N",
        "M06.0", "M06.0L", "M06.0M", "M06.0N",
        "M06.8L", "M06.8M", "M06.8N", "M06.9"
        # intentionally missing M12.3
    ) ~ "RA",
    .data[["diagnoskod_1"]] %in% c(
        "M07.0", "M07.1", "M07.2", "M07.3x+L40.5",
        "M09.0", "M07.0+L40.5", "M07.1+L40.5",
        "M07.2+L40.5"
    ) ~ "PsA",
    .data[["diagnoskod_1"]] %in% c(
        "M46.8", "M46.9", "M08.1"
    ) ~ "SpA",
    .data[["diagnoskod_1"]] %in% c(
        "M31.5", "M31.6", "M35.3"
    ) ~ "GCA",
    .data[["diagnoskod_1"]] %in% c(
        "M13.0", "M13.8"
    ) ~ "Poly-/Oligoartrit",
    .data[["diagnoskod_1"]] %in% "M45.9" ~ "AS",
    .data[["diagnoskod_1"]] %in% "M32.9" ~ "SLE",
    .data[["diagnoskod_1"]] %in% "M08.9" ~ "JIA",
    .data[["diagnoskod_1"]] %in% "M60.9" ~ "Myosit",
    .data[["diagnoskod_1"]] %in% "M10.9" ~ "Gikt",
    .default = "Övriga diagnoser"
)