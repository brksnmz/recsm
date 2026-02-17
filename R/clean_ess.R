clean_ess <- function(path = "ess.csv", countries = c("GB","DE","FR")) {
  miss_small <- c(66, 77, 88, 99)
  readr::read_csv(path, show_col_types = FALSE) |>
    dplyr::filter(cntry %in% countries) |>
    dplyr::mutate(dplyr::across(dplyr::where(is.character), ~ dplyr::na_if(.x, ""))) |>
    dplyr::mutate(dplyr::across(c(ppltrst, agea, nwsptot, netustm, domicil, gndr, eduyrs), as.numeric)) |>
    dplyr::mutate(
      ppltrst = dplyr::if_else(ppltrst > 10 & ppltrst <= 100, ppltrst/10, ppltrst, missing = ppltrst),
      ppltrst = dplyr::if_else(ppltrst %in% miss_small | ppltrst < 0 | ppltrst > 10, NA_real_, ppltrst),
      agea    = dplyr::if_else(agea < 15 | agea > 99, NA_real_, agea),
      nwsptot = dplyr::if_else(nwsptot %in% miss_small, NA_real_, nwsptot),
      netustm = dplyr::if_else(netustm %in% c(7777, 8888, 9999, 99999), NA_real_, netustm),
      eduyrs  = dplyr::if_else(eduyrs %in% miss_small, NA_real_, eduyrs),
      domicil = dplyr::if_else(domicil %in% miss_small, NA_real_, domicil),
      gndr    = dplyr::if_else(gndr %in% miss_small, NA_real_, gndr)
    ) |>
    dplyr::mutate(
      gender = dplyr::recode(gndr, `1` = "Male", `2` = "Female", .default = NA_character_),
      urban = dplyr::case_when(domicil <= 2 ~ "Urban",
                               domicil >= 3 & domicil <= 5 ~ "Non-urban",
                               TRUE ~ NA_character_),
      news_days = nwsptot,
      news_regular = dplyr::if_else(news_days >= 3, 1, 0, missing = NA_real_),
      country = factor(cntry)
    ) |>
    tidyr::drop_na(ppltrst)
}
