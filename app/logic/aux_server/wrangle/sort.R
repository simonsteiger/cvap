box::use(
    magrittr[`%>%`],
    fct = forcats,
    dp = dplyr,
    lub = lubridate,
    sh = shiny,
)

box::use(
    app / logic / srqlib / srqprep,
)

#' @export
get_group <- function(.data, group) {
    colnames(.data) %>%
        `[`(., . %in% group)
}

#' @export
get_target_level <- function(.data, group_var) {
    .data[[group_var]] %>%
        droplevels() %>%
        fct$fct_reorder(-.data[["outcome"]]) %>%
        levels() %>%
        `[`(., 1)
}

#' @export
sort_fct <- function(.data, group) {
    stopifnot(!sh$is.reactive(.data))
    group_var <- get_group(.data, group)

    target_level <- get_target_level(.data, group_var)

    srqprep$prep_custom_order(
        .data,
        .reorder = "lan",
        .by = "outcome",
        .data[[group_var]] == target_level
    )
}

#' @export
sort_date <- function(.data, group) {
    stopifnot(!sh$is.reactive(.data))
    group_var <- get_group(.data, group)

    srqprep$prep_custom_order(
        .data,
        .reorder = "lan",
        .by = "outcome",
        .data[[group_var]] == max(.data[[group_var]])
    )
}

#' @export
sort_num <- function(.data, group) {
    stopifnot(!sh$is.reactive(.data))
    group_var <- get_group(.data, group)

    srqprep$prep_custom_order(
        .data,
        .reorder = "lan",
        .by = "outcome",
        .data[[group_var]] == max(.data[[group_var]])
    )
}

#' @export
sort_scb_id <- function(.data) {
    stopifnot(!sh$is.reactive(.data))
    .data %>%
        dp$mutate(lan = fct$fct_reorder(as.factor(lan), .data[["lan_scb_id"]])) %>%
        dp$arrange(lan) # overridden by table and bar server?
}

#' @export
sort_nogroup <- function(.data) {
    stopifnot(!sh$is.reactive(.data))
    .data %>%
        dp$mutate(lan = fct$fct_reorder(as.factor(lan), .data[["outcome"]])) %>%
        dp$arrange(lan)
}
