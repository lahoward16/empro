library(tidyverse)
source(
  here::here(
    'utils',
    'create_metastatic_subject_flags.R'
  )
)
source(
  here::here(
    'utils',
    'load_proms.R'
  )
)
source(
  here::here(
    'utils',
    'load_sas_medidata.R'
  )
)

## Provide location where the medidata resides
medi_path <- here::here(
  "data",
  "20200522",
  "c16_170_IRONMAN_535_20200515_173636"
)

## Get a subject-level data set that indicates if a subject is metastatic or not
metastatic_subjects <- create_metastatic_subject_flags(medi_path)

## Obtain the medidata object with
medidata_full <- load_sas_medidata(medi_path)

## Check to see if our flagging made sense?
check <- medidata_full$cohort %>% select(Subject, COHORT) %>% left_join(metastatic_subjects, by='Subject')
check <- check %>% mutate_all(replace_na, 0)
table(check$COHORT, check$metastatic)

mCRPC_subjects <- check %>% filter( (COHORT=='CRPC') & (metastatic==1) )

fs::dir_create(here::here("data", "amgen_deliverable", "sas"))
fs::dir_create(here::here("data", "amgen_deliverable", "csv"))

filter_only_mets <- function(data_frame, dfname) {
  temp <- data_frame %>% filter(Subject %in% mCRPC_subjects$Subject)
  eval(
    parse(
      text=(paste0(
        "foreign::write.foreign(
          temp,
          file.path(here::here(\"data\", \"amgen_deliverable\", \"sas\"), '", dfname, ".txt'),
          file.path(here::here(\"data\", \"amgen_deliverable\", \"sas\"), '",dfname,".sas'),
          package = 'SAS', dataname='", dfname, "')"
      ))
    )
  )
  eval(
    parse(
      text=(paste0(
        "write_csv(
        temp,
        file.path(here::here(\"data\", \"amgen_deliverable\", \"csv\"), '",dfname,".csv'))"
      ))
    )
  )
}

map2(medidata_full, names(medidata_full), filter_only_mets)

## Could be going wrong on rule 1 and 2, when searching the data dictionary there is another table, RP_PRMI that
## contains PreText='M Stage' - should this be included? For rule 3, there are many anatomical locations and histologies,
## refining this list may lead to more individuals found to be metastatic. For rule 4, CSOTHER2, I'm coding as
## CSOTHER2!='' as positive?

fs::dir_create(here::here("data", "qc"))
write_csv(
  check,
  here::here("data", "qc", "qc_metastatic_flag.csv")
)

xlsx_path <- here::here(
  "data",
  "20200522",
  "TrueNTH Questionnaire Code Dictionary - IRONMAN.xlsx"
)

proms_data_file <- here::here(
  "data",
  "20200522",
  "TrueNTH_qnr-data-2020_05_19-12_41.csv"
)

proms_list <- load_proms(xlsx_path, proms_data_file)

# Subset to only those requested
proms_wide <- proms_list$proms_wide %>%
  rename(Subject=study_id) %>%
  filter(Subject %in% mCRPC_subjects$Subject)

proms <- proms_list$proms %>%
  rename(Subject=study_id) %>%
  filter(Subject %in% mCRPC_subjects$Subject)
## The rest of this lifted from data-provisioning_Pernar, except that I use Subject
## instead of study_id variable
out_wide <- left_join(
  proms_wide,
  medidata_full$dm %>%
    select(Subject, AGE),
  by = "Subject"
) %>%
  left_join(
    medidata_full$cohort %>%
      select(Subject, COHORT, DENOVOMET_DISEASE),
    by = "Subject"
  ) %>%
  left_join(
    medidata_full$ic %>%
      mutate(CNSNTDATE = as_date(CNSTDATE)) %>%
      select(Subject, CNSNTDATE),
    by = "Subject"
  ) %>%
  janitor::clean_names()

foreign::write.foreign(
  out_wide,
  file.path(
    here::here('data', 'amgen_deliverable', 'sas'), 'proms_wide.txt'
  ),
  file.path(
    here::here('data', 'amgen_deliverable', 'sas'), 'proms_wide.sas'
  ),
  package = 'SAS',
  dataname = 'proms_wide'
)

write_csv(
  out_wide,
  file.path(here::here("data", "amgen_deliverable", "csv"), 'proms_wide.csv')
)

out <- left_join(
  proms,
  medidata_full$dm %>%
    select(Subject, AGE),
  by = "Subject"
) %>%
  left_join(
    medidata_full$cohort %>%
      select(Subject, COHORT, DENOVOMET_DISEASE),
    by = "Subject"
  ) %>%
  left_join(
    medidata_full$ic %>%
      mutate(CNSNTDATE = as_date(CNSTDATE)) %>%
      select(Subject, CNSNTDATE),
    by = "Subject"
  ) %>%
  left_join(
    medidata_full$treatment %>%
      mutate(EXSTDAT = as_date(EXSTDAT)) %>%
      select(Subject, TREATMENT, EXSTDAT),
    by = "Subject"
  ) %>%
  mutate(
    authored = as.numeric(authored - CNSNTDATE),
    EXSTDAT = as.numeric(EXSTDAT - CNSNTDATE),
    CNSNTDATE = as.numeric(CNSNTDATE - CNSNTDATE)
  ) %>%
  janitor::clean_names()

foreign::write.foreign(
  out,
  file.path(
    here::here('data', 'amgen_deliverable', 'sas'), 'proms_long.txt'
  ),
  file.path(
    here::here('data', 'amgen_deliverable', 'sas'), 'proms_long.sas'
  ),
  package = 'SAS',
  dataname = 'proms_long'
)

write_csv(
  out,
  file.path(here::here("data", "amgen_deliverable", "csv"), 'proms_long.csv')
)
