box::use(
    hw = htmlwidgets,
)

#' @export
#' wraps a JavaScript function which accesses name and value parameters and formats them
#' value is expected to be Län
format_decimal <- function() {
    hw$JS("
            function(params){
                return('<b>Län:</b> ' + params.name + '<br /><b>Värde:</b> ' + params.value)
            }
        ")
}

#' @export
#' wraps a JavaScript function which accesses name and value parameters and formats them
#' value is expected to be Län
format_percent <- function() {
    hw$JS("
            function(params){
                return('<b>Län:</b> ' + params.name + '<br /><b>Värde:</b> ' + params.value + ' %')
            }
        ")
}

#' @export
format_year <- function() {
    hw$JS("
        {yyyy}
    ")
}

#' @export
#' wraps the previously defined formatters so they can be subsetted based on function args
format_list <- list(
    decimal = format_decimal,
    percent = format_percent,
    year = format_year
)