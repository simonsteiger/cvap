box::use(
    sh = shiny,
)

box::use(
    aui = app / logic / aux_ui,
)

head <- function(id, title) {
    aui$row2(
        class = "row py-4 m-4 d-flex justify-content-center align-items-center",
        colwidths = list(2, 8, 2),
        content = list(
            sh$div(aui$btn_return(id)),
            sh$div(class = "fs-1 h-font text-center", title),
            sh$div(class = "justify-content-end", sh$img(src = "static/logo_wide.png", width = "100%"))
        )
    )
}
