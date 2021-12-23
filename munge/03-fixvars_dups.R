
# sexage <- sexage %>% group_by(patientreference) %>% slice(1) %>% ungroup()
rsdata <- left_join(rsdata,
  sexage %>% select(patientreference, SEX, DATE_OF_BIRTH),
  by = c("PATIENTREFERENCE" = "patientreference")
) %>%
  filter(!is.na(SEX))

rsdata <- rsdata %>%
  mutate(
    d_DATE_FOR_ADMISSION = coalesce(DATE_FOR_ADMISSION, VISIT_DATE),
    indexyear = year(d_DATE_FOR_ADMISSION),
    age = as.numeric(floor((d_DATE_FOR_ADMISSION - DATE_OF_BIRTH) / 365.25)),
    sex = factor(case_when(
      SEX == "FEMALE" ~ "Kvinnna",
      SEX == "MALE" ~ "Man"
    )),
    tmp_timedurationhf = d_DATE_FOR_ADMISSION - DATE_FOR_DIAGNOSIS_HF,
    tmp_timedurationhf2 = case_when(
      tmp_timedurationhf < 6 * 30.5 ~ "LESS_THAN_6_MONTHS",
      tmp_timedurationhf >= 6 * 30.5 ~ "MORE_THAN_6_MONTHS"
    ),
    hfdur = coalesce(tmp_timedurationhf2, DURATION_OF_HF),
    hfdur = case_when(
      hfdur == "LESS_THAN_6_MONTHS" ~ "Duration HF < 6 månader",
      hfdur == "MORE_THAN_6_MONTHS" ~ "Duration HF > 6 månader"
    ),
    ef_cat = factor(case_when(
      LVEF_SEMIQUANTITATIVE == "NORMAL" | LVEF_PERCENT >= 50 ~ 1,
      LVEF_SEMIQUANTITATIVE == "MILD" | LVEF_PERCENT >= 40 ~ 1,
      LVEF_SEMIQUANTITATIVE == "MODERATE" | LVEF_PERCENT >= 30 ~ 2,
      LVEF_SEMIQUANTITATIVE == "SEVERE" | LVEF_PERCENT < 30 ~ 2
    ), labels = c(">=40", "<40"), levels = 1:2),
    ef_cat3 = factor(case_when(
      LVEF_SEMIQUANTITATIVE == "NORMAL" | LVEF_PERCENT >= 50 ~ 1,
      LVEF_SEMIQUANTITATIVE == "MILD" | LVEF_PERCENT >= 40 ~ 2,
      LVEF_SEMIQUANTITATIVE == "MODERATE" | LVEF_PERCENT >= 30 ~ 3,
      LVEF_SEMIQUANTITATIVE == "SEVERE" | LVEF_PERCENT < 30 ~ 3
    ), labels = c("HFpEF", "HFmrEF", "HFrEF"), levels = 1:3),
    efcrt_cat = factor(case_when(
      LVEF_SEMIQUANTITATIVE %in% c("NORMAL", "MILD") | LVEF_PERCENT > 35 ~ 1,
      LVEF_SEMIQUANTITATIVE %in% c("MODERATE", "SEVERE") | LVEF_PERCENT <= 35 ~ 2
    ), labels = c(">=40/>35", "<40/<=35"), levels = 1:2),
    FUNCTION_CLASS_NYHA = str_replace(FUNCTION_CLASS_NYHA, "_", " ")
  )

# Clean 2 duplicates from index -------------------------------------------

# koll2 <- rsdata %>%
#   filter(TYPE == "YEARLY_FOLLOWUP") %>%
#   group_by(PATIENTREFERENCE, indexyear) %>%
#   slice(2) %>%
#   ungroup() # NOT OK


# koll2 <- rsdata %>%
#   filter(TYPE == "FOLLOWUP") %>%
#   group_by(PATIENTREFERENCE, indexyear) %>%
#   slice(2) %>%
#   ungroup() # OK
#
# koll2 <- rsdata %>%
#   filter(TYPE == "INDEX") %>%
#   group_by(PATIENTREFERENCE, indexyear) %>%
#   slice(2) %>%
#   ungroup() # 2 dups

rsdatafollow <- rsdata %>%
  filter(TYPE == "FOLLOWUP") %>%
  group_by(PATIENTREFERENCE, indexyear) %>%
  arrange(d_DATE_FOR_ADMISSION) %>%
  slice(1) %>%
  ungroup()

rsdatayfollow <- rsdata %>%
  filter(TYPE == "YEARLY_FOLLOWUP") %>%
  group_by(PATIENTREFERENCE, indexyear) %>%
  arrange(d_DATE_FOR_ADMISSION) %>%
  slice(1) %>%
  ungroup()

rsdataindex <- rsdata %>%
  filter(TYPE == "INDEX") %>%
  group_by(PATIENTREFERENCE) %>%
  arrange(MIGRATED) %>%
  slice(1) %>%
  ungroup()

rsdata <- bind_rows(
  rsdataindex,
  rsdatafollow,
  rsdatayfollow
)



# Create variable EF at index ---------------------------------------------

rsdataindex <- rsdata %>%
  filter(
    TYPE == "INDEX"
  ) %>%
  transmute(
    PATIENTREFERENCE = PATIENTREFERENCE,
    d_DATE_FOR_ADMISSIONindex = d_DATE_FOR_ADMISSION,
    ef_catindex = ef_cat,
    efcrt_catindex = efcrt_cat,
    FUNCTION_CLASS_NYHAindex = FUNCTION_CLASS_NYHA,
    QRS_WIDTHindex = QRS_WIDTH,
    LEFT_BRANCH_BLOCKindex = LEFT_BRANCH_BLOCK,
    hfdurindex = hfdur
  )

rsdata <- left_join(rsdata,
  rsdataindex,
  by = "PATIENTREFERENCE"
) %>%
  mutate(
    # most current qrs, lbbb, nyha
    QRS_WIDTHimp = coalesce(QRS_WIDTH, QRS_WIDTHindex),
    LEFT_BRANCH_BLOCKimp = coalesce(LEFT_BRANCH_BLOCK, LEFT_BRANCH_BLOCKindex),
    FUNCTION_CLASS_NYHAimp = coalesce(FUNCTION_CLASS_NYHA, FUNCTION_CLASS_NYHAindex),

    # smallest ef
    ef_catimp = case_when(
      ef_cat == "<40" | ef_catindex == "<40" ~ "<40",
      ef_cat == ">=40" | ef_catindex == ">=40" ~ ">=40"
    ),
    efcrt_catimp = case_when(
      efcrt_cat == "<40/<=35" | efcrt_catindex == "<40/<=35" ~ "<40/<=35",
      efcrt_cat == ">=40/>35" | efcrt_catindex == ">=40/>35" ~ ">=40/>35"
    ),
    ttype = case_when(
      TYPE == "INDEX" ~ 1,
      TYPE == "FOLLOWUP" ~ 2,
      TYPE == "YEARLY_FOLLOWUP" ~ 3
    ),
    tmp_timeadmission = as.numeric(d_DATE_FOR_ADMISSION - d_DATE_FOR_ADMISSIONindex),
    ttype = if_else(tmp_timeadmission >= 1.5 * 365 & TYPE == "YEARLY_FOLLOWUP", 4, ttype),
    ttype = factor(ttype, levels = 1:4, labels = c("Index", "6v-6mån uppföljning", "1års uppföljning", "2+års uppföljning"))
  )
