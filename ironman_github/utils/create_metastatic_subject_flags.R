library(tidyverse)
source(here::here("utils", "load_sas_medidata.R"))

create_metastatic_subject_flags <- function(medi_path) {
  medidata <- load_sas_medidata(medi_path)

  # TODO: Is there a way to change this to use mhclinstg_m_stage vector as opposed to explicitly calling each element out?
  mhclinstg_m_stage <- c(
    "CLINSTG_MSTAGE_UICC02",
    "CLINSTG_MSTAGE_UICC09",
    "CLINSTG_MSTAGE_UICC92",
    "CLINSTG_MSTAGE_UICC97",
    "CLINSTG_MSTAGE_UNK",
    "CLINSTG_MSTAGE_UICC16"
  )
  m_stage_conditions <- c("M1", "M1a", "M1b", "M1c")
  rule_1 <- medidata$mhclinstg %>%
    mutate(first_rule = case_when(
      ((is.na(CLINSTG_MSTAGE_UICC02)) & (is.na(CLINSTG_MSTAGE_UICC09)) & (is.na(CLINSTG_MSTAGE_UICC92)) & (is.na(CLINSTG_MSTAGE_UICC97)) & (is.na(CLINSTG_MSTAGE_UNK)) & (is.na(CLINSTG_MSTAGE_UICC16))) ~ NA_real_,
      CLINSTG_MSTAGE_UICC02 %in% m_stage_conditions ~ 1,
      CLINSTG_MSTAGE_UICC09 %in% m_stage_conditions ~ 1,
      CLINSTG_MSTAGE_UICC92 %in% m_stage_conditions ~ 1,
      CLINSTG_MSTAGE_UICC97 %in% m_stage_conditions ~ 1,
      CLINSTG_MSTAGE_UNK %in% m_stage_conditions ~ 1,
      CLINSTG_MSTAGE_UICC16 %in% m_stage_conditions ~ 1,
      TRUE ~ 0
    )) %>%
    mutate(origin_CLINSTG_MSTAGE_UICC02 = ifelse(CLINSTG_MSTAGE_UICC02 %in% m_stage_conditions, "CLINSTG_MSTAGE_UICC02", "")) %>%
    mutate(origin_CLINSTG_MSTAGE_UICC09 = ifelse(CLINSTG_MSTAGE_UICC09 %in% m_stage_conditions, "CLINSTG_MSTAGE_UICC09", "")) %>%
    mutate(origin_CLINSTG_MSTAGE_UICC92 = ifelse(CLINSTG_MSTAGE_UICC92 %in% m_stage_conditions, "CLINSTG_MSTAGE_UICC92", "")) %>%
    mutate(origin_CLINSTG_MSTAGE_UICC97 = ifelse(CLINSTG_MSTAGE_UICC97 %in% m_stage_conditions, "CLINSTG_MSTAGE_UICC97", "")) %>%
    mutate(origin_CLINSTG_MSTAGE_UNK = ifelse(CLINSTG_MSTAGE_UNK %in% m_stage_conditions, "CLINSTG_MSTAGE_UNK", "")) %>%
    mutate(origin_CLINSTG_MSTAGE_UICC16 = ifelse(CLINSTG_MSTAGE_UICC16 %in% m_stage_conditions, "CLINSTG_MSTAGE_UICC16", "")) %>%
    select(Subject, first_rule, one_of(paste0("origin_", mhclinstg_m_stage))) %>% # In future will propogate reason
    select(Subject, first_rule) %>%
    group_by(Subject) %>%
    slice(which.max(first_rule)) %>%
    ungroup()

  # Second rule: medidata$prpros, need to check the "M-stage Variables":
  # TODO: Is there a way to change this to use prpros_m_stage vector as opposed to explicitly calling each element out?
  prpros_m_stage <- c(
    "RPDTL_MSTAGE_UICC02",
    "RPDTL_MSTAGE_UICC09",
    "RPDTL_MSTAGE_UICC97",
    "RPDTL_MSTAGE_UICC92",
    "RPDTL_MSTAGE_UNK",
    "RPDTL_MSTAGE_UICC16"
  )
  rule_2 <- medidata$prpros %>%
    mutate(second_rule = case_when(
      ((is.na(RPDTL_MSTAGE_UICC02)) & (is.na(RPDTL_MSTAGE_UICC09)) & (is.na(RPDTL_MSTAGE_UICC92)) & (is.na(RPDTL_MSTAGE_UICC97)) & (is.na(RPDTL_MSTAGE_UNK)) & (is.na(RPDTL_MSTAGE_UICC16))) ~ NA_real_,
      RPDTL_MSTAGE_UICC02 %in% m_stage_conditions ~ 1,
      RPDTL_MSTAGE_UICC09 %in% m_stage_conditions ~ 1,
      RPDTL_MSTAGE_UICC92 %in% m_stage_conditions ~ 1,
      RPDTL_MSTAGE_UICC97 %in% m_stage_conditions ~ 1,
      RPDTL_MSTAGE_UNK %in% m_stage_conditions ~ 1,
      RPDTL_MSTAGE_UICC16 %in% m_stage_conditions ~ 1,
      TRUE ~ 0
    )) %>%
    mutate(origin_RPDTL_MSTAGE_UICC02 = ifelse(RPDTL_MSTAGE_UICC02 %in% m_stage_conditions, "RPDTL_MSTAGE_UICC02", "")) %>%
    mutate(origin_RPDTL_MSTAGE_UICC09 = ifelse(RPDTL_MSTAGE_UICC09 %in% m_stage_conditions, "RPDTL_MSTAGE_UICC09", "")) %>%
    mutate(origin_RPDTL_MSTAGE_UICC92 = ifelse(RPDTL_MSTAGE_UICC92 %in% m_stage_conditions, "RPDTL_MSTAGE_UICC92", "")) %>%
    mutate(origin_RPDTL_MSTAGE_UICC97 = ifelse(RPDTL_MSTAGE_UICC97 %in% m_stage_conditions, "RPDTL_MSTAGE_UICC97", "")) %>%
    mutate(origin_RPDTL_MSTAGE_UNK = ifelse(RPDTL_MSTAGE_UNK %in% m_stage_conditions, "RPDTL_MSTAGE_UNK", "")) %>%
    mutate(origin_RPDTL_MSTAGE_UICC16 = ifelse(RPDTL_MSTAGE_UICC16 %in% m_stage_conditions, "RPDTL_MSTAGE_UICC16", "")) %>%
    select(Subject, second_rule, one_of(paste0("origin_", prpros_m_stage))) %>% # In future will propogate reason
    select(Subject, second_rule) %>%
    group_by(Subject) %>%
    slice(which.max(second_rule)) %>%
    ungroup()

  # Third rule:
  location <- c(
    "Bladder",
    "Bone Marrow",
    "Ilium",
    "Kidney",
    "Liver",
    "Lung",
    "Mediastinum",
    "Pelvis",
    "Retroperitoneum",
    "Spine",
    "Anus",
    "Colon",
    "Other"
  )
  rule_3 <- medidata$mhdiagbx %>%
    mutate(third_rule = case_when(
      ((is.na(DIAGBX_SITE)) | (is.na(DIAGBX_HISTOLOGY))) ~ NA_real_,
      ((DIAGBX_SITE %in% location) & (DIAGBX_HISTOLOGY != "Benign")) ~ 1,
      TRUE ~ 0
    )) %>%
    mutate(origin_DIAGBX_SITE_HISTOLOGY = ifelse(((DIAGBX_SITE %in% location) & (DIAGBX_HISTOLOGY != "Benign")), "mhdiagbx.DIAGBX_SITE_HITSOLOGY", "")) %>%
    select(Subject, third_rule, origin_DIAGBX_SITE_HISTOLOGY) %>% # In future will propogate reason
    select(Subject, third_rule) %>%
    group_by(Subject) %>%
    slice(which.max(third_rule)) %>%
    ungroup()


  # Fourth rule:
  # TODO: Is there a way to change this to use cs_variables vector as opposed to explicitly calling each element out?

  cs_variables <- c("CSSKULL", "CSTHOR", "CSSPINE", "CSPELVIS", "CSEXT", "CSLIVER", "CSLUNG", "CSDN", "CSOTH")

  rule_4 <- medidata$cs %>%
    mutate(fourth_rule = case_when(
      CSSKULL == "Yes" ~ 1,
      CSTHOR == "Yes" ~ 1,
      CSSPINE == "Yes" ~ 1,
      CSPELVIS == "Yes" ~ 1,
      CSEXT == "Yes" ~ 1,
      CSLIVER == "Yes" ~ 1,
      CSLUNG == "Yes" ~ 1,
      CSDN == "Yes" ~ 1,
      CSOTH == "Yes" ~ 1,
      CSOTHER2 != "" ~ 1,
      TRUE ~ 0
    )) %>%
    mutate(origin_CSSKULL = ifelse(CSSKULL == "Yes", "CSSKULL", "")) %>%
    mutate(origin_CSTHOR = ifelse(CSTHOR == "Yes", "CSTHOR", "")) %>%
    mutate(origin_CSSPINE = ifelse(CSSPINE == "Yes", "CSSPINE", "")) %>%
    mutate(origin_CSPELVIS = ifelse(CSPELVIS == "Yes", "CSPELVIS", "")) %>%
    mutate(origin_CSEXT = ifelse(CSEXT == "Yes", "CSEXT", "")) %>%
    mutate(origin_CSLIVER = ifelse(CSLIVER == "Yes", "CSLIVER", "")) %>%
    mutate(origin_CSLUNG = ifelse(CSLUNG == "Yes", "CSLUNG", "")) %>%
    mutate(origin_CSDN = ifelse(CSDN == "Yes", "CSDN", "")) %>%
    mutate(origin_CSOTH = ifelse(CSOTH == "Yes", "CSOTH", "")) %>%
    mutate(origin_CSOTHER2 = ifelse(CSOTHER2 != "", "CSOTHER2", "")) %>%
    select(Subject, fourth_rule, one_of(paste0("origin_", cs_variables))) %>% # in future will propogate origination of flagging
    select(Subject, fourth_rule) %>%
    group_by(Subject) %>%
    slice(which.max(fourth_rule)) %>%
    ungroup()


  # Fifth Rule
  # TODO: Is there a way to change this to use cs_variables vector as opposed to explicitly calling each element out?
  table_names_rule_5 <- c("new_pq", "new_pq_v4", "new_pq_v5")
  variable_names_rule_5_new_pq <- c(
    "NEWPQ_2C",
    "NEWPQ_2B",
    "NEWPQ_2D",
    "NEWPQ_2E"
  )
  variable_names_rule_5_new_pq_v4 <- c(
    "NEWPQ12B_V4",
    "NEWPQ12C_V4",
    "NEWPD12D_V4",
    "NEWPQ12E_V4"
  )
  variable_names_rule_5_new_pq_v5 <- c(
    "NEWPQ12B_V5",
    "NEWPQ12C_V5",
    "NEWPQ12D_V5",
    "NEWPQ12E_V5"
  )

  rule5_1 <- medidata$new_pq %>%
    mutate(fifth_rule_1 = case_when(
      NEWPQ_2C == 1 ~ 1,
      NEWPQ_2B == 1 ~ 1,
      NEWPQ_2D == 1 ~ 1,
      NEWPQ_2E == 1 ~ 1,
      TRUE ~ 0
    )) %>%
    select(Subject, fifth_rule_1) %>%
    group_by(Subject) %>%
    slice(which.max(fifth_rule_1)) %>%
    ungroup()

  rule5_2 <- medidata$new_pq_v4 %>%
    mutate(fifth_rule_2 = case_when(
      NEWPQ12B_V4 == 1 ~ 1,
      NEWPQ12C_V4 == 1 ~ 1,
      NEWPQ12D_V4 == 1 ~ 1,
      NEWPQ12E_V4 == 1 ~ 1,
      TRUE ~ 0
    )) %>%
    select(Subject, fifth_rule_2) %>%
    group_by(Subject) %>%
    slice(which.max(fifth_rule_2)) %>%
    ungroup()

  # TODO: this data set is empty need to confirm with rest of team
  rule5_3 <- medidata$new_pq_v5 %>%
    mutate(fifth_rule_3 = case_when(
      NEWPQ12B_V5 == 1 ~ 1,
      NEWPQ12C_V5 == 1 ~ 1,
      NEWPQ12D_V5 == 1 ~ 1,
      NEWPQ12E_V5 == 1 ~ 1,
      TRUE ~ 0
    )) %>%
    select(Subject, fifth_rule_3) %>%
    group_by(Subject) %>%
    slice(which.max(fifth_rule_3)) %>%
    ungroup()

  metastatic_flags <- rule_1 %>%
    full_join(rule_2, by = "Subject") %>%
    full_join(rule_3, by = "Subject") %>%
    full_join(rule_4, by = "Subject") %>%
    full_join(rule5_1, by = "Subject") %>%
    full_join(rule5_2, by = "Subject") %>%
    full_join(rule5_3, by = "Subject") %>%
    mutate(metastatic = case_when(
      first_rule == 1 ~ 1,
      second_rule == 1 ~ 1,
      third_rule == 1 ~ 1,
      fourth_rule == 1 ~ 1,
      fifth_rule_1 == 1 ~ 1,
      fifth_rule_2 == 1 ~ 1,
      fifth_rule_3 == 1 ~ 1,
      TRUE ~ 0
    ))

  return(metastatic_flags)
}
