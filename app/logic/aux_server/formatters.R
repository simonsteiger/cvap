box::use(
    hw = htmlwidgets,
)

#' @export
format_decimal <- function() {
    hw$JS("
            function(params){
                return('<b>Län:</b> ' + params.name + '<br /><b>Värde:</b> ' + params.value)
            }
        ")
}

#' @export
format_percent <- function() {
    hw$JS("
            function(params){
                return('<b>Län:</b> ' + params.name + '<br /><b>Värde:</b> ' + params.value + ' %')
            }
        ")
}

#' @export
format_list <- list(
    decimal = format_decimal,
    percent = format_percent
)