
ProjectTemplate::reload.project(munge = F, data_loading = F)

memory.limit(size = 10000000000000)

# Import data from UCR ----------------------------------------------------

newrs <- read_sas("./raw-data/RiksSvikt_Datauttag_Årsrapport_DAT-619/datauttag_arsrapport.sas7bdat")

# Centre for new rs --------------------------------------------------------

center <- read_sas("./raw-data/Lev4/Data/rikssvikt-ng/base/rikssvikt_ou.sas7bdat")

#center <- clean_data(center)

sexage <- read_sas("./raw-data/Lev4/Data/rikssvikt-ng/base/rikssvikt_pd_dodsdatum.sas7bdat")

# Store as RData in /data folder ------------------------------------------

save(file = "./data/rawData_rs.RData", list = c(
  "newrs", "center", "sexage"
))

# tg
prevtime <- read.xlsx("./raw-data/tg/10981_2021 RiksSvikt - Prev mot Prev 2010-2019 - Match Huvuddiagnos 2021-05-17 Lev2_klar_LB.xlsx", 
          sheet = "Län per år")
inctime <- read.xlsx("./raw-data/tg/10981_2021 RiksSvikt - Täckningsgrad 2003-2019 - Registrering inom 1 år 2021-05-24 Lev3_klar_LB.xlsx", 
                     sheet = "Län per år")
prev2019 <- read.xlsx("./raw-data/tg/10981_2021 RiksSvikt - Prev mot Prev 2010-2019 - Match Huvuddiagnos 2021-05-17 Lev2_klar_LB.xlsx", 
                      sheet = "2019")
inc2019 <- read.xlsx("./raw-data/tg/10981_2021 RiksSvikt - Täckningsgrad 2003-2019 - Registrering inom 1 år 2021-05-24 Lev3_klar_LB.xlsx", 
                    sheet = "2019")

prev2019 <- prev2019 %>% as_tibble(.name_repair = "unique")
inc2019 <- inc2019 %>% as_tibble(.name_repair = "unique")

save(file = "./data/rawData_tg.RData", list = c(
  "prevtime", "inctime", "prev2019", "inc2019" 
))

# Get map data ------------------------------------------------------------

swedenmap <- getData("GADM", country="SWE", level=1)

saveRDS(swedenmap, file = "./data/swedenmap.rds")

