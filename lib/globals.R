# default is to use tidyverse functions
select <- dplyr::select
rename <- dplyr::rename
filter <- dplyr::filter
mutate <- dplyr::mutate
complete <- tidyr::complete

global_colsblue <- c(
"#9BC7E5",
"#6FAFDA", 
"#5296D1", 
"#1358BA", 
"#0F36A3", 
"#002282", 
"#001B66", 
"#021447"
)

global_colsgreymiss <- "#C2C2C2"

global_colslimit <- c("#32a543", "#fdb823")

# year of report
global_startdtm <- ymd("2021-01-01")
global_stopdtm <- ymd("2021-12-31")
global_year <- 2021
