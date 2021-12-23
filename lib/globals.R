# default is to use tidyverse functions
select <- dplyr::select
rename <- dplyr::rename
filter <- dplyr::filter
mutate <- dplyr::mutate
complete <- tidyr::complete

# colours
global_cols <- c(
  # "#56B4E9",
  "#88D6EE",
  "#1D6903",
  "#FF8702"
)

global_colsblue <- c(
  "#1E6170",
  "#397C93",
  "#4E9FBA",
  "#88D6EE",
  "#B7E6F5",
  "#E5F7FD"
)

global_colsmiss <- "#A9A9A9"

# year of report
start <- ymd("2021-01-01")
stop <- ymd("2021-12-31")
ar <- 2021
