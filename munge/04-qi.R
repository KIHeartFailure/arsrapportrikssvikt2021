rsdata <- rsdata %>%
  mutate(
    qi_lvef = if_else(!is.na(LVEF_PERCENT) | !is.na(LVEF_SEMIQUANTITATIVE), 1, 0),
    qi_ntprobnp = if_else(!is.na(NT_PROBNP) | !is.na(NT_PROBNP_24H), 1, 0),
    qi_nyha = if_else(!is.na(FUNCTION_CLASS_NYHA), 1, 0),
    qi_qol = if_else(!is.na(LIFEQUALITY_SCORE), 1, 0),
    qi_uppfhf = case_when(
      FOLLOWUP_UNIT == "DECEASED" ~ NA_real_,
      is.na(FOLLOWUP_HF_UNIT) ~ NA_real_,
      FOLLOWUP_HF_UNIT == "YES" ~ 1,
      FOLLOWUP_HF_UNIT == "NO" ~ 0
    ),
    qi_ras = case_when(
      FOLLOWUP_UNIT == "DECEASED" ~ NA_real_,
      is.na(ef_catimp) | ef_catimp != "<40" ~ NA_real_,
      is.na(ACE_INHIBITOR) | is.na(A2_BLOCKER_ARB) | is.na(ARNI) ~ NA_real_,
      ACE_INHIBITOR == "YES" | A2_BLOCKER_ARB == "YES" | ARNI == "YES" ~ 1,
      TRUE ~ 0
    ),
    qi_arni = case_when(
      is.na(qi_ras) ~ NA_real_,
      ARNI == "YES" ~ 1,
      TRUE ~ 0
    ),
    qi_bbl = case_when(
      FOLLOWUP_UNIT == "DECEASED" ~ NA_real_,
      is.na(ef_catimp) | ef_catimp != "<40" ~ NA_real_,
      is.na(BETA_BLOCKER) ~ NA_real_,
      BETA_BLOCKER == "YES" ~ 1,
      TRUE ~ 0
    ),
    qi_mra = case_when(
      FOLLOWUP_UNIT == "DECEASED" ~ NA_real_,
      is.na(ef_catimp) | ef_catimp != "<40" ~ NA_real_,
      is.na(MRA) ~ NA_real_,
      MRA == "YES" ~ 1,
      TRUE ~ 0
    ),
    qi_trippel = case_when(
      is.na(qi_ras) | is.na(qi_bbl) | is.na(qi_mra) ~ NA_real_,
      qi_ras + qi_bbl + qi_mra == 3 ~ 1,
      TRUE ~ 0
    ),
    qi_sglt2 = case_when(
      FOLLOWUP_UNIT == "DECEASED" ~ NA_real_,
      is.na(SGLT2) ~ NA_real_,
      SGLT2 == "YES" ~ 1,
      TRUE ~ 0
    ),

    # crt
    qi_crt = case_when(
      is.na(efcrt_catimp) | efcrt_catimp != "<40/<=35" ~ NA_real_,
      is.na(QRS_WIDTHimp) | is.na(LEFT_BRANCH_BLOCKimp) | is.na(FUNCTION_CLASS_NYHAimp) ~ NA_real_,
      # FOLLOWUP_UNIT == "DECEASED" ~ NA_real_,
      TYPE == "INDEX" & hfdur == "Duration HF < 6 månader" ~ NA_real_,
      DEVICE_THERAPY %in% c("CRT", "CRT_D") ~ 1,
      QRS_WIDTHimp >= 150 & LEFT_BRANCH_BLOCKimp == "YES" & FUNCTION_CLASS_NYHAimp %in% c("NYHA II", "NYHA III", "NYHA IV") ~ 0
    ),

    # icd
    qi_icd = case_when(
      is.na(efcrt_catimp) | efcrt_catimp != "<40/<=35" ~ NA_real_,
      is.na(FUNCTION_CLASS_NYHAimp) ~ NA_real_,
      # FOLLOWUP_UNIT == "DECEASED" ~ NA_real_,
      TYPE == "INDEX" & hfdur == "Duration HF < 6 månader" ~ NA_real_,
      DEVICE_THERAPY %in% c("ICD", "CRT_D") ~ 1,
      FUNCTION_CLASS_NYHAimp %in% c("NYHA II", "NYHA III", "NYHA IV") ~ 0
    ),
    qi_fys = case_when(
      FOLLOWUP_UNIT == "DECEASED" ~ NA_real_,
      is.na(PARTICIPATION_HF_TRAINING) ~ NA_real_,
      PARTICIPATION_HF_TRAINING == "YES" ~ 1,
      TRUE ~ 0
    )
  )


# uppföljningar

follow <- rsdata %>%
  filter(TYPE == "FOLLOWUP") %>%
  mutate(followup = 1) %>%
  select(PATIENTREFERENCE, followup)

rsdata <- left_join(rsdata, follow, by = "PATIENTREFERENCE")

rsdata <- rsdata %>%
  mutate(qi_followreg = case_when(
    FOLLOWUP_UNIT == "DECEASED" | TYPE != "INDEX" ~ NA_real_,
    followup == 1 ~ 1,
    TRUE ~ 0
  ))
