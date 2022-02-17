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

# global_colsblue <- rev(c(
#   #"#F0FAFF",
#   #"#D6F0F7",
#   "#9BD4E5",
#   "#70C1DA",
#   "#4FB3D1",
#   "#2F99BA",
#   "#0F83A3",
#   "#006E8A",
#   "#034F69",
#   "#023647"
# ))
# 
 global_colsgreen <- c(
   "#144702",
   "#1C6603"
 )
#   "#9AD24A",
#   "#78BA1C",
#   "#61A60F",
#   "#408400", 
#   "#1C6603", 
#   "#144702"
# ))

global_colsgreymiss <- "#C2C2C2"

global_colslimit <- c("#32a543", "#fdb823")

# year of report
global_startdtm <- ymd("2021-01-01")
global_stopdtm <- ymd("2021-12-31")
global_year <- 2021
