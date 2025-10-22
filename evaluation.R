library(dplyr)
library(tidyr)
library(ggplot2)
library(purrr)
library(readr)




# ---------------------------------------------------------------------
# 1. Combine all methods into one tidy long data frame
# ---------------------------------------------------------------------
relMSE_long <- map2_dfr(
  method_dfs,
  names(method_dfs),
  ~ .x %>%
    mutate(repeat = row_number()) %>%
    pivot_longer(
      cols = starts_with("size_"),
      names_to = "size",
      values_to = "relMSE"
    ) %>%
    mutate(
      method = .y,
      size = as.numeric(sub("size_", "", size))
    )
)
