library(tidyverse)
# Assuming that the file create_deliverable.R was run, it may need some clean up of file names
# This would be done if someone didn't want to share a full path on their system

remove_text <- here::here() %>%
  str_remove('"')
replace_text <- 'foreign::write.foreign(.*)'

deliverable_path <- here::here(
    'data',
    'amgen_deliverable',
    'sas'
)

files <- dir(deliverable_path, pattern = "*.sas")

clean_file_paths_from_file <- function(file) {
  text <- readLines(file)
  text <- str_remove(text, remove_text)
  writeLines(text, file)
}

files %>%
  file.path(deliverable_path, .) %>%
  map(clean_file_paths_from_file)
