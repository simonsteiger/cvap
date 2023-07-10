box::use(
    app / logic / aux_server / obs_return,
    app / logic / aux_server / sift,
    app / logic / aux_server / spell,
)

#' @export
obs_return <- obs_return$obs_return

#' @export
sift_cols <- sift$sift_cols

#' @export
sift_vars <- sift$sift_vars

#' @export
spell_kon <- spell$spell_kon

#' @export
spell_alder <- spell$spell_alder

#' @export
spell_outcome <- spell$spell_outcome

#' @export
spell_period <- spell$spell_period

#' @export
spell_dxcat <- spell$spell_dxcat

#' @export
spell_prep_typ <- spell$spell_prep_typ

#' @export
create_subtitle <- spell$create_subtitle