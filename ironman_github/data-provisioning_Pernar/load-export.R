library(tidyverse)
library(lubridate)

date_of_data_refresh <- "20201027"

# PROMs -------------------------------------------------------------------

source(here::here("utils", "load_proms.R"))

proms <- load_proms(
  here::here(
    "data",
    date_of_data_refresh,
    "TrueNTH Questionnaire Code Dictionary - IRONMAN_v2.0.xlsx"
  ),
  here::here(
    "data",
    date_of_data_refresh,
    "qnr-data-2020_10_27-23_06.csv"
  )
)

# 5 patients have series of responses with missing `timepoint`
# filtering these out for now
# There is also a problem where some patients have multiple responses for the same
# question at the same timepoint. For example:
# proms %>% filter((study_id == "170-11-019" &
#                     question_code == "irondemog_v3.25" &
#                     timepoint == "Baseline") |
#                  (study_id == "170-96-025" &
#                     question_code %in% c("irondemog_v3.13", "irondemog_v3.25") &
#                     timepoint == "Month 3")) %>%
#   View()
# I'm going to set equal to the first response that appears for now

proms_wide <- proms %>%
  group_by(study_id, timepoint, question_code) %>%
  slice(1) %>%
  ungroup() %>%
  filter(!is.na(timepoint)) %>%
  select(study_id, timepoint, question_code, answer_text) %>%
  pivot_wider(
    names_from = question_code,
    values_from = answer_text,
    values_fn = list(answer_text = first)
  )

# Medidata ----------------------------------------------------------------

source(here::here("utils", "load_sas_medidata.R"))
medidata <- load_sas_medidata(
  here::here("data", date_of_data_refresh, "c16_170_IRONMAN_586_20201027_173647")
)

# join wide
out_wide <- left_join(
  proms_wide,
  medidata$dm %>%
    rename(study_id = Subject) %>%
    select(study_id, AGE),
  by = "study_id"
) %>%
  left_join(
    medidata$cohort %>%
      rename(study_id = Subject) %>%
      select(study_id, COHORT, DENOVOMET_DISEASE),
    by = "study_id"
  ) %>%
  left_join(
    medidata$ic %>%
      mutate(CNSNTDATE = as_date(CNSTDATE)) %>%
      rename(study_id = Subject) %>%
      select(study_id, CNSNTDATE),
    by = "study_id"
  ) %>%
  janitor::clean_names()

foreign::write.foreign(
  out_wide,
  file.path(
    here::here("data-provisioning_Pernar", "exports"), 'proms_wide-20200718.txt'
  ),
  file.path(
    here::here("data-provisioning_Pernar", "exports"), 'proms_wide-20200718.sas'
  ),
  package = 'SAS',
  dataname = 'proms_wide'
)

write_csv(
  out_wide,
  here::here("data-provisioning_Pernar", "exports", "proms_wide-20200718.csv"),
  na = " "
)
