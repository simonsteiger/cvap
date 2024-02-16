box::use(
    dp = dplyr,
    lub = lubridate,
    magrittr[`%>%`],
)

#' @export
set_utsatt <- function(.data) {
    .data %>%
        dp$mutate(
            status = ifelse(is.na(utsatt), 0, 1),
            status = ifelse(
                !is.na(avslutad) & !is.na(utsatt) & avslutad <= utsatt,
                0, status
            ),
            utsatt = lub$as_date(
                ifelse(status == 0, pmin(avslutad, lub$today() + lub$dmonths(1), na.rm = TRUE), utsatt)
            ),
            pagaende = ifelse(utsatt < lub$today(), 0, pagaende)
        ) %>%
        dp$select(-status)
}
