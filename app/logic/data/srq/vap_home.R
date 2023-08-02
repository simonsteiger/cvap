box::use(
    magrittr[`%>%`],
    fst,
    dp = dplyr,
    lub = lubridate,
    pr = purrr,
)

box::use(
    ski = app / logic / swissknife / skinit,
)

ski$read_dir("/Users/simonsteiger/Desktop/data/fst/")

vis_t1 <- list_df$besoksdata %>%
    dp$filter(datum < "2022-01-01") %>%
    dp$pull(besoks_id) %>%
    dp$n_distinct()

#' @export
vis_t2 <- list_df$besoksdata %>%
    dp$filter(datum < "2023-01-01") %>%
    dp$pull(besoks_id) %>%
    dp$n_distinct()

#' @export
vis_diff <- vis_t2 - vis_t1

pat_t1 <- list_df$basdata %>%
    dp$filter(inkluderad < "2022-01-01") %>%
    dp$pull(patientkod) %>%
    dp$n_distinct()

#' @export
pat_t2 <- list_df$basdata %>%
    dp$filter(inkluderad < "2023-01-01") %>%
    dp$pull(patientkod) %>%
    dp$n_distinct()

#' @export
pat_diff <- pat_t2 - pat_t1