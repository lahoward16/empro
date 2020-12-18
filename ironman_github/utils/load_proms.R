library(tidyverse)
library(lubridate)

load_proms <- function(proms_metadata_path, proms_data_path) {
  # question codes exist in the even numbered sheets; read and row bind them
  q_codes <- map(seq(2, 16, by = 2), ~ readxl::read_xlsx(proms_metadata_path, sheet = .x)) %>%
    map_dfr(~.) %>%
    rename(
      "question_code" = "Question Code",
      "question_text" = "Question Text"
    )

  # question options exist in odd numbered sheets; read and row bind them
  # sheet 15 has erroneously named columns - these are handled separately
  q_opts <- map(seq(3, 21, by = 2)[-6], ~ readxl::read_xlsx(proms_metadata_path, sheet = .x)) %>%
    map_dfr(~.) %>%
    bind_rows(readxl::read_xlsx(proms_metadata_path, sheet = 13) %>%
      rename(
        "Option Code" = "Question Code",
        "Option Text" = "Question Text"
      )) #%>%
    #separate(answer_code, sep = "[.]", into = c(NA, NA, "answer_order"), remove = FALSE)

  proms <- read_csv(proms_data_path, guess_max = 200000) %>%
    left_join(q_codes, by = "question_code") %>%
    left_join(q_opts, by = "answer_code") %>%
    select(-identifier, -truenth_subject_id, -site_id) %>%
    select(-starts_with("X")) # there are stray data elements in columns 17:30

  # repair missing question text fields with question code
  # replace missing answer text fill-in options or spaces
  proms <- proms %>%
    mutate(
      question_text = coalesce(question_text, question_code),
      question_text = str_replace(question_text, "<strong>(.+?)</strong>", "\\1"),
      question_text = str_replace(question_text, "<b>(.+?)</b>", "\\1"),
      answer_text = coalesce(answer_text, other_text),
      answer_text = replace_na(answer_text, ""),
      answer_text = str_replace(answer_text, "<u>(.+?)</u>", "\\1")
    )

  # most dates have format 2019-11-13T15:23:43Z but a few have 5/30/2019 22:00
  proms <- proms %>%
    separate(authored, into = "authored", sep = " ", extra = "drop") %>%
    mutate(
      authored =
        if_else(
          str_detect(authored, "/"),
          mdy(authored),
          as_date(authored)
        )
    )

  return(proms)
}
