box::use(
    sh = shiny,
    dp = dplyr,
    bsl = bslib,
    pr = purrr,
    rl = rlang,
)

box::use(
    aui = app / logic / aux_ui
)

icon_kon <- function(input, ...) {
    sh$tagList(
        dp$case_match(input,
            "Kvinna" ~ sh$tagList(sh$icon("venus")),
            "Man" ~ sh$tagList(sh$icon("mars")),
            .default = sh$tagList(sh$icon("venus-mars"))
        ),
        input
    )
}

icon_alder <- function(input, ...) {
    sh$tagList(
        dp$case_when(
            max(input) < 40 ~ sh$tagList(sh$icon("children")),
            min(input) > 70 ~ sh$tagList(sh$icon("person-cane")),
            .default = sh$tagList(sh$icon("people-group"))
        ),
        paste0(paste0(input, collapse = " till "), " år")
    )
}

icon_date <- function(input, ...) {
    sh$tagList(
        sh$icon("calendar"), paste0(input, collapse = " till ")
    )
}

icon_lan <- function(input, ...) {
    sh$tagList(
        sh$icon("map-location-dot"), paste0(input, collapse = ", ")
    )
}

icon_dxcat <- function(input, ...) {
    sh$tagList(
        sh$icon("clipboard-list"), paste0(input, collapse = ", ")
    )
}

icon_lan_modal <- function(input, ...) {
    dots <- rl$list2(...)
    cond_one <- length(unlist(input)) > 1
    cond_all <- length(unlist(input)) == 21
    res <- pr$map(input, \(x) sh$tags$li(x))

    sh$tagList(
        sh$div(
            class = "d-flex justify-content-between align-items-center",
            sh$div(
                sh$icon("map-location-dot"),
                paste0(
                    if (cond_all) "Alla" else length(input),
                    " län"
                )
            ),
            aui$btn_modal(
                id = dots$id,
                label = sh$tagList(sh$icon("list-ul"), "Se lista"),
                modal_title = if (cond_one) "Utvalda län" else "Utvalt län",
                footer_confirm = NULL,
                footer_dismiss = NULL,
                sh$div(
                    class = "m-2",
                    bsl$layout_column_wrap(
                        width = 1 / 3, height = 300,
                        !!!res
                    )
                )
            )
        )
    )
}

icon_lan_popover <- function(input, ...) {
    sh$tagList(
        sh$icon("map-location-dot"),
        aui$btn_popover(
            title = paste0(length(input), " län utvald"),
            content = paste0(input, collapse = ", ")
        )
    )
}

make_icon <- list(
    kon = icon_kon,
    alder = icon_alder,
    inkluderad = icon_date,
    ordinerat = icon_date,
    ongoing = icon_date,
    lan = icon_lan_modal,
    dxcat = icon_dxcat
)

#' @export
ui <- function(id) {
    ns <- sh$NS(id)
    sh$tagList()
}

#' @export
server <- function(id) {
    sh$moduleServer(id, function(input, output, session) {
        inputs_with_icons <- names(input)[names(input) %in% names(make_icon)]

        # Iterate over all input names that have a corresponding icon-function
        # If current name is last name, make icon only
        # Else, make icon and hr tag
        pr$map(
            inputs_with_icons,
            \(name) {
                if (which(names(input) == name) == length(inputs_with_icons)) {
                    make_icon[[name]](input[[name]], id = id)
                } else {
                    sh$tagList(make_icon[[name]](input[[name]]), sh$hr())
                }
            }
        )
    })
}
