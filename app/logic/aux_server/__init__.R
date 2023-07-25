box::use(
    app / logic / aux_server / obs_return,
    app / logic / aux_server / sift,
    app / logic / aux_server / spells,
    app / logic / aux_server / icons,
    app / logic / aux_server / errors,
    app / logic / aux_server / formatters,
    app / logic / aux_server / checks,
)

#' @export
obs_return <- obs_return$obs_return

#' @export
sift_cols <- sift$sift_cols

#' @export
sift_vars <- sift$sift_vars

#' @export
maybe_lookback <- sift$maybe_lookback

#' @export
maybe_ongoing <- sift$maybe_ongoing

#' @export
create_subtitle <- spells$create_subtitle

#' @export
iconostasis <- icons$iconostasis

#' @export
error_no_data <- errors$error_no_data

#' @export
format_decimal <- formatters$format_decimal

#' @export
format_percent <- formatters$format_percent

#' @export
format_list <- formatters$format_list

#' @export
vali_date <- checks$vali_date