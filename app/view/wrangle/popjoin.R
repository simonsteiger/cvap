box::use(
    sh = shiny,
    dp = dplyr,
    lub = lubridate,
    magrittr[`%>%`],
)

#' @export
server <- function(id, sifted, pop) {
    sh$moduleServer(id, function(input, output, session) {
        stopifnot(sh$is.reactive(sifted))
        stopifnot(sh$is.reactive(pop))

        sh$reactive({
            sifted_year <- dp$mutate(sifted(), Year = lub$year(ongoing_timestamp))
            pop() %>%
                dp$filter(Year %in% lub$year(input$ongoing)) %>%
                dp$right_join(sifted_year, by = c("lan", "Year")) %>%
                dp$select(-Year)
        })
    })
}