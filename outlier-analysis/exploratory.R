##########################################
##### Contact Definition Exploratory #####
##########################################

df_contact <- readRDS("outlier-analysis/data/df_contact.RDS")

contacts_site <- df_contact %>%
  dplyr::group_by(rec_id, study_site, study_day, participant_age, participant_sex) %>%
  dplyr::summarize(num_contacts = n())

