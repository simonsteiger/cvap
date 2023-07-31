box::use(
    sh = shiny,
    pr = purrr,
    str = stringr,
    magrittr[`%>%`],
)

icd_helper <- function(icd) {
    pr$map(icd, \(x) sh$tags$li(x)) %>% sh$tags$ul()
}

#' @export
icd_compose <- function(icd) {
    switch(str$str_extract(icd$header, "(?<=\\()\\w+(?=\\))"), # get string in brackets
        "RA" = sh$tagList(sh$tags$h5(icd$header), icd_helper(icd$codes)),
        "AS" = sh$tagList(sh$tags$h5(icd$header), icd_helper(icd$codes)),
        "SpA" = sh$tagList(sh$tags$h5(icd$header), icd_helper(icd$codes)),
        "PsA" = sh$tagList(sh$tags$h5(icd$header), icd_helper(icd$codes)),
        stop(paste0("Unknown header (diagnosis) to icd_compose()"))
    )
}
