
# Add centers -------------------------------------------------------------

rsdata <- left_join(rsdata,
  center %>%
    select(ID, ORG_UNIT_NAME, PARENT1, PARENT2),
  by = c("HEALTH_CARE_UNIT_REFERENCE" = "ID")
)


# Add region ------------------------------------------------------------

rsdata <- left_join(rsdata,
  center %>%
    filter(DEPTH == 1) %>%
    rename(region = ORG_UNIT_NAME) %>%
    select(ID, region),
  by = c("PARENT1" = "ID")
) %>%
  mutate(
    region = str_remove(region, "Region "),
    region = str_remove(region, " län"),
    region = str_remove(region, "sregionen"),
    region = if_else(region == "Jönköpings", "Jönköping", region),
    region = if_else(region == "Sörmanland", "Sörmland", region)
  )


# Add hospital --------------------------------------------------------------

rsdata <- left_join(rsdata,
  center %>%
    filter(DEPTH == 2) %>%
    rename(tmp_center = ORG_UNIT_NAME) %>%
    select(ID, tmp_center),
  by = c("PARENT2" = "ID")
) %>%
  mutate(center = case_when(
    ORG_UNIT_LEVEL_NAME %in% c("Fristående hjärtmottagning", "Vårdcentral") ~ ORG_UNIT_NAME,
    TRUE ~ tmp_center
  ))


# Group VC ect ------------------------------------------------------------

rsdata <- rsdata %>%
  mutate(
    vtype = factor(case_when(
      ORG_UNIT_LEVEL_NAME %in% c("Avdelning", "Fristående hjärtmottagning", "Mottagning") ~ 1,
      ORG_UNIT_LEVEL_NAME %in% c("Vårdcentral") ~ 2
    ), levels = 1:2, labels = c("Sjukhus", "Primärvård")) # ,
    # center = case_when(
    #  ORG_UNIT_LEVEL_NAME %in% c("Vårdcentral") ~ "",
    # ORG_UNIT_LEVEL_NAME %in% c("Fristående hjärtmottagning") ~ "Fristående enhet",
    #  TRUE ~ center
    #  )#,
    # i regioner ingår ej vc
    # regionvc = region,
    # region = case_when(
    #  ORG_UNIT_LEVEL_NAME %in% c("Vårdcentral") ~ "",
    #  TRUE ~ region
    # )
  )


# Manual hårdkodning ------------------------------------------------------

rsdata <- rsdata %>%
  mutate(
    center = case_when(
      center %in% c("Sahlgrenska Universitetssjukhuset - Sahlgrenska", 
                    "Sahlgrenska Universitetssjukhuset - Östra", 
                    "Sahlgrenska Universitetssjukhuset - Mölndal") ~ 
                      "Sahlgrenska Universitetssjukhuset",
      TRUE ~ center
    ),
    region = factor(region),
    center = factor(center)
  )
