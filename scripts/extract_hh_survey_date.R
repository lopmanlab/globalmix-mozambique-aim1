# 1. extract rec_id and and hh_id from the participant file
hhid <- participant_data_aim2 %>%
  select(rec_id, hh_id, study_site) %>%
  distinct(hh_id, .keep_all = TRUE)

# 2. extract rec_id and survey_date from contacts file
survey_date <- contact_data_aim2 %>%
  select(rec_id, survey_date) %>%
  distinct(rec_id, .keep_all = TRUE)

# 3. merge
hh_survey_date <- hhid %>%
  left_join(survey_date, by="rec_id")

write.csv(hh_survey_date, file="data/clean/hh_survey_date.csv", row.names = FALSE)
