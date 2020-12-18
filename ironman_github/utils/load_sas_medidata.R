library(tidyverse)

load_sas_medidata <- function(medi_path) {
  files <- dir(medi_path, pattern = "*.sas7bdat")
  # this one has a problem right now - need this one and working in latest delivery.
  #files <- files[-which(files == "new_pq_v5.sas7bdat")]
  # for me, this one fails as well
  files <- files[-which(files == "newdisc_pqv5.sas7bdat")]

  medidata <- files %>%
    file.path(medi_path, .) %>%
    set_names(str_replace(files, ".sas7bdat", "")) %>%
    map(haven::read_sas)

  return(medidata)
}