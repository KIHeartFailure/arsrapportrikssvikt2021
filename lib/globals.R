# default is to use tidyverse functions
select <- dplyr::select
rename <- dplyr::rename
filter <- dplyr::filter
mutate <- dplyr::mutate
complete <- tidyr::complete

global_colsblue <- rev(c(
  #"#F0FAFF",
  #"#D6F0F7",
  "#9BD4E5",
  "#70C1DA",
  "#4FB3D1",
  "#2F99BA",
  "#0F83A3",
  "#006E8A",
  "#034F69",
  "#023647"
))

global_colsgreen <- rev(c(
  "#C6E59B",
  "#AEDA70",
  "#9AD24A",
  "#78BA1C",
  "#61A60F",
  "#408400", 
  "#1C6603", 
  "#144702"
))

global_colsgrey <- rev(c(
  "#C2C2C2",
  "#A6A6A6",
  "#8C8C8C",
  "#666666",
  "#4D4D4D",
  "#333333",
  "#262626",
  "#1A1A1A"
))

# year of report
global_startdtm <- ymd("2021-01-01")
global_stopdtm <- ymd("2021-12-31")
global_year <- 2021
