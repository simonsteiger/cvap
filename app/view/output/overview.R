box::use(
    sh = shiny,
    dp = dplyr,
    bsl = bslib,
    pr = purrr,
)

icon_kon <- function(input) {
    sh$tagList(
        dp$case_match(input,
            "Kvinna" ~ sh$tagList(sh$icon("venus")),
            "Man" ~ sh$tagList(sh$icon("mars")),
            .default = sh$tagList(sh$icon("venus-mars"))
        ),
        input
    )
}

icon_alder <- function(input) {
    sh$tagList(
        dp$case_when(
            max(input) < 40 ~ sh$tagList(sh$icon("children")),
            min(input) > 70 ~ sh$tagList(sh$icon("person-cane")),
            .default = sh$tagList(sh$icon("people-group"))
        ),
        paste0(paste0(input, collapse = " till "), " år")
    )
}

icon_date <- function(input) {
    sh$tagList(
        sh$icon("calendar"), paste0(input, collapse = " till ")
    )
}

icon_lan <- function(input) {
    sh$tagList(
        sh$icon("location-dot"), paste0(input, collapse = ", ")
    )
}

make_icon <- list(
    kon = icon_kon,
    alder = icon_alder,
    inkluderad = icon_date,
    ordinerat = icon_date,
    lan = icon_lan
)

#' @export
ui <- function(id) {
    ns <- sh$NS(id)
    sh$tagList()
}

#' @export
server <- function(id) {
    sh$moduleServer(id, function(input, output, session) {
        pr$map(names(input)[names(input) %in% names(make_icon)], \(name) {
            if (which(names(input) == name) == length(names(input))) {
                make_icon[[name]](input[[name]])
            } else {
                sh$tagList(make_icon[[name]](input[[name]]), sh$hr())
            }
        })
    })
}

# test_fn_icon_kon <- function(input) {
#     sh$tagList(
#         dp$case_match(input,
#             "Kvinna" ~ sh$tagList(sh$icon("venus")),
#             "Man" ~ sh$tagList(sh$icon("mars")),
#             .default = sh$tagList(sh$icon("venus-mars"))
#         ),
#         paste0(paste0(input, collapse = " till "), " år")
#     )
# }
# #
# test_fn_icon_alder <- function(input) {
#     sh$tagList(
#         dp$case_when(
#             max(input) < 40 ~ sh$tagList(sh$icon("children")),
#             min(input) > 70 ~ sh$tagList(sh$icon("person-cane")),
#             .default = sh$tagList(sh$icon("people-group"))
#         ),
#         paste0(paste0(input, collapse = " till "), " år")
#     )
# }
# #
# test_fn_icon_date <- function(input) {
#     sh$tagList(
#         sh$icon("calendar"), paste0(input, collapse = " till ")
#     )
# }
# 
# test_icon_lan <- function(input) {
#     sh$tagList(
#         sh$icon("location-dot"), paste0(input, collapse = ", ")
#     )
# }
# #
# test_fns_icon <- list(
#     kon = test_fn_icon_kon,
#     alder = test_fn_icon_alder,
#     inkluderad = test_fn_icon_date,
#     ordinerat = test_fn_icon_date,
#     lan = test_icon_lan
# )
# #
# input <- list(kon = "Man", alder = c(18, 100), ordinerat = c("2020-01-01", "2021-01-01"), lan = c("Östra Götaland", "VVV"))
# #
# res <- pr$map(names(input), \(name) {
#     if (which(names(input) == name) == length(names(input))) {
#         test_fns_icon[[name]](input[[name]])
#     } else {
#         sh$tagList(test_fns_icon[[name]](input[[name]]), sh$hr())
#     }
# })
# 
# pr$map(
#     names(input), \(name) ifelse(which(names(input) == name) == length(names(input)), TRUE, FALSE)
# )
# #
# 
# 