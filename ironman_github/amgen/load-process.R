library(tidyverse)
library(lubridate)

xlsx_path <- here::here("data",
                        "20200522",
                        "TrueNTH Questionnaire Code Dictionary - IRONMAN.xlsx")

q_codes <- readxl::read_xlsx(xlsx_path, sheet = 2) %>%
  bind_rows(readxl::read_xlsx(xlsx_path, sheet = 4)) %>%
  bind_rows(readxl::read_xlsx(xlsx_path, sheet = 6)) %>%
  bind_rows(readxl::read_xlsx(xlsx_path, sheet = 8)) %>%
  bind_rows(readxl::read_xlsx(xlsx_path, sheet = 10)) %>%
  bind_rows(readxl::read_xlsx(xlsx_path, sheet = 12)) %>%
  bind_rows(readxl::read_xlsx(xlsx_path, sheet = 14)) %>%
  bind_rows(readxl::read_xlsx(xlsx_path, sheet = 16)) %>%
  rename("question_code" = "Question Code",
         "question_text" = "Question Text")

q_opts <- readxl::read_xlsx(xlsx_path, sheet = 3) %>%
  bind_rows(readxl::read_xlsx(xlsx_path, sheet = 5)) %>%
  bind_rows(readxl::read_xlsx(xlsx_path, sheet = 7)) %>%
  bind_rows(readxl::read_xlsx(xlsx_path, sheet = 9)) %>%
  bind_rows(readxl::read_xlsx(xlsx_path, sheet = 11)) %>%
  bind_rows(readxl::read_xlsx(xlsx_path, sheet = 13)) %>%
  bind_rows(readxl::read_xlsx(xlsx_path, sheet = 15) %>%
              rename("Option Code" = "Question Code",
                     "Option Text" = "Question Text")) %>%
  bind_rows(readxl::read_xlsx(xlsx_path, sheet = 17)) %>%
  rename("answer_code" = "Option Code",
         "answer_text" = "Option Text")

proms <- here::here("data",
                    "20200522",
                    "TrueNTH_qnr-data-2020_05_19-12_41.csv") %>%
  read_csv(guess_max = 200000) %>%
  left_join(q_codes, by = "question_code") %>%
  left_join(q_opts, by = "answer_code") %>%
  select(-identifier, -truenth_subject_id, -site_id) %>%
  select(-starts_with("X")) # there are stray data elements in columns 17:30

# repair missing question text fields with question code
# replace missing answer text fill-in options or spaces
proms <- proms %>%
  mutate(question_text = coalesce(question_text, question_code)) %>%
  mutate(question_text = str_replace(question_text, "<strong>(.+?)</strong>", "\\1")) %>%
  mutate(question_text = str_replace(question_text, "<b>(.+?)</b>", "\\1")) %>%
  mutate(answer_text = coalesce(answer_text, other_text)) %>%
  mutate(answer_text = replace_na(answer_text, "")) %>%
  mutate(answer_text = str_replace(answer_text, "<u>(.+?)</u>", "\\1"))

# most dates have format 2019-11-13T15:23:43Z but a few have 5/30/2019 22:00 (detectable by space)
proms <- proms %>% 
  group_split(str_detect(authored, " ")) %>% 
  modify_at(1, ~mutate(., authored = as_date(authored))) %>% 
  modify_at(2, ~separate(., authored, into = "authored", sep = " ", extra = "drop") %>% 
              mutate(authored = mdy(authored))) %>% 
  bind_rows() %>% 
  select(-"str_detect(authored, \" \")")


# load medidata
medi_path <- here::here("data", "20200522", "c16_170_IRONMAN_535_20200515_173636")
files <- dir(medi_path, pattern = "*.sas7bdat")
# this one has a problem right now
files <- files[-which(files == "new_pq_v5.sas7bdat")]
# for me, this one fails as well
files <- files[-which(files == "newdisc_pqv5.sas7bdat")]

medidata <- files %>% 
  file.path(medi_path, .) %>% 
  set_names(str_replace(files, ".sas7bdat", "")) %>%
  map(haven::read_sas) 

# to do: follow amgen rules, then join proms and medidata
# First, follow modified version of Non-ADT Tx Lines Algorithm.docx

# Merge 'treatment' with 'cohort' where COHORT=='CRPC',
# TODO: modify to make this mCRPC
# According to our meeting, a complicated algorithm is needed to identify the metastatic portion of this cohort,
# so will proceded without this identifier for now...I'll first subset to ensure no duplicate columns pulled as there
# is quite a bit of overlap (with column names)
medidata_cohort <- medidata$cohort %>%
  select(
    subjectId,
    Subject,
    COHORT,
    StudySiteId, #will probably want this for certain analyses to account for heirarchy / design
    DENOVOMET_DISEASE #this is an interesting varaible, can it help with metastatic portion of our cohort
  ) %>%
  filter(COHORT=='CRPC')


medidata_trt <- medidata$treatment %>%
  select(
    subjectId,
    TREATMENT,
    EXSTDAT,
    EXENDAT,
    EXONGOING,
    EXDOSE,
    EXUNIT,
    EXDISREASON,
    InstanceName,
    Folder,
    EXDOSE, # could be interesting in a cumulative dose analysis
    EXUNIT # to help with normalization (if needed)
  )

medidata_cohort_trt <- medidata_cohort %>%
  left_join(medidata_trt, by='subjectId') %>%
  arrange(subjectId, EXSTDAT)

# Step 2: Remove all ADT and Steroid Treatments, really we'll create a flag and describe how many of these we have
adt_excel_path <- here::here(
  "data",
  "20200522",
  "ADT_v1_05-21-2020.xlsx"
)

ADT_METADATA <- readxl::read_xlsx(adt_excel_path, sheet = 1)
ADT_TREATMENT_VECTOR <-  ADT_METADATA %>% filter(Type=='ADT') %>% select(TREATMENT) %>% pull()
STEROID_TREATMENT_VECTOR <-  ADT_METADATA %>% filter(Type=='Steroid') %>% select(TREATMENT) %>% pull()


medidata_cohort_trt <- medidata_cohort_trt %>%
  mutate(TREATMENT_TYPE = case_when(
    TREATMENT %in% ADT_TREATMENT_VECTOR ~ 'ADT',
    TREATMENT %in% STEROID_TREATMENT_VECTOR ~ 'Steroid',
    is.na(TREATMENT) ~ 'Missing Treatment',
    TRUE ~ 'NON-ADT'
  ))

medidata_cohort_trt_non_adt_baseline <- medidata_cohort_trt %>%
  filter(TREATMENT_TYPE=='NON-ADT') %>%
  filter(grepl('Baseline', InstanceName, ignore.case=TRUE))

medidata_cohort_trt_non_adt_baseline_count <- medidata_cohort_trt_non_adt_baseline %>%
  group_by(subjectId) %>%
  tally()

medidata_cohort_trt_non_adt_baseline_count_vector <- medidata_cohort_trt_non_adt_baseline_count %>%
  select(subjectId) %>%
  pull()

#helper function
`%notin%` <- Negate(`%in%`)

# These are individuals that don't have baseline information!
check <- medidata_cohort_trt %>%
  filter(subjectId %notin% medidata_cohort_trt_non_adt_baseline_count_vector) %>%
  filter(TREATMENT_TYPE=='NON-ADT')
#table(check$InstanceName)

#numbers for consort diagram
medidata_cohort_trt_consort <- medidata_cohort_trt %>%
  arrange(subjectId, TREATMENT_TYPE) %>%
  group_by(subjectId, TREATMENT_TYPE) %>%
  summarise(TOTAL_TREATMENT_TYPE = sum(n())) %>%
  ungroup()

# These are all the subjectId's that have a "Missing Treatment"
missing_treatement_type_vector <- medidata_cohort_trt_consort %>%
  filter(TREATMENT_TYPE=='Missing Treatment') %>%
  select(subjectId) %>%
  pull()

# Here is a tibble that contains all individuals that have a "missing Treatment" - looking to see other Treatmetn Types
missing_treatement_type <- medidata_cohort_trt_consort %>%
  filter(subjectId %in% missing_treatement_type_vector)
#print(table(missing_treatement_type$TREATMENT_TYPE))

medidata_at_least_1_ADT <- medidata_cohort_trt_consort %>%
  filter(TREATMENT_TYPE=='ADT') %>%
  tally(n())

medidata_at_least_1_Steroid <- medidata_cohort_trt_consort %>%
  filter(TREATMENT_TYPE=='Steroid') %>%
  tally(n())

medidata_at_least_1_NonADT <- medidata_cohort_trt_consort %>%
  filter(TREATMENT_TYPE=='NON-ADT') %>%
  tally(n())

medidata_at_least_1_Missing <- medidata_cohort_trt_consort %>%
  filter(TREATMENT_TYPE=='Missing Treatment') %>%
  tally(n())

consort <- data.frame(
  'Label' = c(
    paste0('Total Patients (n=', nrow(medidata$cohort), ') \n Total Treatments (n=', nrow(medidata$treatment), ')'),
    paste0('Exclude all mHSPC \n (n=', table(medidata$cohort$COHORT)['mHSPC'], ')\n AND All Missing \n (n=', table(medidata$cohort$COHORT)[1], ')'),
    paste0('Include all CRPC \n (n=', table(medidata$cohort$COHORT)['CRPC'], ')'),
    paste0('At least 1 \n ADT Treatment \n (n= ', medidata_at_least_1_ADT, ')'),
    paste0('At least 1 \n NON-ADT Treatment \n (n=', medidata_at_least_1_NonADT, ')'),
    paste0('At least 1 Steroid Treatment \n (n=', medidata_at_least_1_Steroid, ')'),
    paste0('At least 1 Missing Treatment \n (n=', medidata_at_least_1_Missing, ')'),
    paste0('Has Baseline Data Available \n (n=', nrow(medidata_cohort_trt_non_adt_baseline_count), ')')
  )
)
consort_path <- here::here(
  "data",
  "consort_diagram_numbers.csv"
)
write.csv(
  consort,
  consort_path
)