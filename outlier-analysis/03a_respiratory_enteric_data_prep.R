rm(list=ls())
library(here)
library(dplyr)
library(ggplot2)
library(plotly)
library(tidyverse)

participants <- readRDS(paste0(here(),"/data/clean/participant_data_aim1.RDS"))
contacts <- readRDS(paste0(here(),"/data/clean/contact_data_aim1.RDS"))

## Subset contacts to the IDs in participant list only
contacts <- contacts %>%
  dplyr::filter(rec_id %in% unlist(participants$rec_id))

# Filter contact relationship == self (you cannot have a contact with yourself)
contacts <- contacts %>%
  dplyr::filter(hh_member_relationship != "Self")

# relationship
contacts$hh_membership <- factor(contacts$hh_membership,
                                 levels = c("Member", "Non-member"))


# recategorize contact locations
contacts <- contacts %>%
  mutate(cnt_home = ifelse(location_contact___0==1, 1,0),
         cnt_school = ifelse(location_contact___2==1, 1,0),
         cnt_work = ifelse(location_contact___3==1, 1,0),
         cnt_otherplace = ifelse(location_contact___1==1 | 
                                   location_contact___4==1 | 
                                   location_contact___5==1 | 
                                   location_contact___6==1 | 
                                   location_contact___7==1 |
                                   location_contact___8==1 | 
                                   location_contact___9==1 |
                                   location_contact___10==1 | 
                                   location_contact___11==1,1,0))
# masking
contacts <- contacts %>%
  mutate(contact_mask2 = case_when(contact_mask == "Yes, for the entire encounter" ~ "Yes",
                                   contact_mask == "Yes, during parts of encounter" ~ "Yes",
                                   contact_mask == "No mask was worn during the encounter" ~ "No",
                                   TRUE ~ "Can't recall"))
contacts$contact_mask2 <- factor(contacts$contact_mask2, levels = c("Yes", "No",
                                                                    "Can't recall"))

contacts$cnt_home <- factor(contacts$cnt_home, levels = c(1, 0),
                            labels = c("Yes", "No"))
contacts$cnt_work <- factor(contacts$cnt_work, levels = c(1, 0),
                            labels = c("Yes", "No"))
contacts$cnt_school <- factor(contacts$cnt_school, levels = c(1, 0),
                              labels = c("Yes", "No"))
contacts$cnt_otherplace <- factor(contacts$cnt_otherplace, levels = c(1, 0),
                                  labels = c("Yes", "No"))

df_contact <- contacts %>%
  dplyr::select(-study_site) %>%
  left_join(participants, by=("rec_id"))

df_contact <- df_contact %>%
  mutate(duration = case_when(duration_contact == "<5 mins" ~ 0,
                              duration_contact == "5-15 mins" ~ 1,
                              duration_contact == "16-30 mins" ~ 2,
                              duration_contact == "31 mins-1 hr" ~ 3,
                              duration_contact == "1-4 hrs" ~ 4,
                              duration_contact  == ">4 hrs" ~ 5))
df_contact <- df_contact %>%
  mutate(respiratory = ifelse(cnt_home == "Yes", 1,
                              ifelse(where_contact != "Outdoors" & duration >= 2, 1, 
                                     ifelse(where_contact == "Outdoors" & duration >= 4, 1, 0)
                                     )
                              )
         ) %>%
  mutate(enteric = ifelse(cnt_home == "Yes", 1,
                          ifelse(where_contact != "Outdoors" & touch_contact == "Yes", 1,
                                 ifelse(where_contact != "Outdoors" & touch_contact == "No" & duration >= 2, 1,
                                        ifelse(where_contact == "Outdoors" & touch_contact == "Yes" & duration >= 2, 1, 
                                               0 )
                                        )
                                 )
                          )
         )

contacts_resp <- df_contact %>%
  dplyr::group_by(rec_id, study_site, study_day, participant_age, participant_sex, age, respiratory) %>%
  dplyr::summarize(num_contacts = n())

contact_unique_resp <- contacts_resp %>%
  filter(respiratory == 1) %>%
  dplyr::group_by(rec_id, study_site, participant_age, participant_sex, age) %>%
  
  summarise(avg_unique_resp_contacts = (mean(num_contacts)))

contacts_resp <- df_contact %>%
  dplyr::group_by(rec_id, fromdayone, respiratory, study_site, participant_age, participant_sex, age) %>%
  dplyr::summarize(num_contacts = n())

contacts_daily_resp <- tidyr::pivot_wider(contacts_resp %>% filter(respiratory == 1), 
                                     names_from = fromdayone, values_from=num_contacts)
contacts_daily_resp$`Both Days`[which(is.na(contacts_daily_resp$`Both Days`))] <- 0
contacts_daily_resp$`Day1 Only`[which(is.na(contacts_daily_resp$`Day1 Only`))] <- 0
contacts_daily_resp$`Day2 Only`[which(is.na(contacts_daily_resp$`Day2 Only`))] <- 0
contacts_daily_resp$`NA`[which(is.na(contacts_daily_resp$`NA`))] <- 0
contacts_daily_resp$avg_daily_resp_contacts <- ((contacts_daily_resp$`Both Days` * 2)+
                                                  contacts_daily_resp$`Day1 Only` + 
                                                  contacts_daily_resp$`Day2 Only`+
                                                  contacts_daily_resp$`NA`)/2


contacts_ent <- df_contact %>%
  dplyr::group_by(rec_id, study_site, study_day, participant_age, participant_sex, age, enteric) %>%
  dplyr::summarize(num_contacts = n())

contact_unique_ent <- contacts_ent %>%
  filter(enteric == 1) %>%
  dplyr::group_by(rec_id, study_site, participant_age, participant_sex, age) %>%
  
  summarise(avg_unique_ent_contacts = (mean(num_contacts)))

contacts_ent <- df_contact %>%
  dplyr::group_by(rec_id, fromdayone, enteric, study_site, participant_age, participant_sex, age) %>%
  dplyr::summarize(num_contacts = n())

contacts_daily_ent <- tidyr::pivot_wider(contacts_ent %>% filter(enteric == 1), 
                                     names_from = fromdayone, values_from=num_contacts)
contacts_daily_ent$`Both Days`[which(is.na(contacts_daily_ent$`Both Days`))] <- 0
contacts_daily_ent$`Day1 Only`[which(is.na(contacts_daily_ent$`Day1 Only`))] <- 0
contacts_daily_ent$`Day2 Only`[which(is.na(contacts_daily_ent$`Day2 Only`))] <- 0
contacts_daily_ent$`NA`[which(is.na(contacts_daily_ent$`NA`))] <- 0
contacts_daily_ent$avg_daily_ent_contacts <- ((contacts_daily_ent$`Both Days` * 2)+
                                                contacts_daily_ent$`Day1 Only` + 
                                                contacts_daily_ent$`Day2 Only`+
                                                contacts_daily_ent$`NA`)/2

# One main dataset for all outcomes ---------------------------------------------

contacts_resp_ent <- left_join(contact_unique_resp, 
                               contact_unique_ent %>% 
                                 ungroup() %>%
                                 dplyr::select(rec_id, avg_unique_ent_contacts),
                               by = c("rec_id" = "rec_id")) %>%
  left_join(., contacts_daily_resp %>% ungroup() %>%
              dplyr::select(rec_id, avg_daily_resp_contacts),
            by = c("rec_id" = "rec_id")) %>%
  left_join(., contacts_daily_ent %>% ungroup() %>%
              dplyr::select(rec_id, avg_daily_ent_contacts),
            by = c("rec_id" = "rec_id"))

# Outlier variables ------------------------------------------------------------
contact_summaries <- contacts_resp_ent %>%
  distinct() %>%
  ungroup() %>%
  dplyr::summarize(unique_resp_q75 = quantile(avg_unique_resp_contacts, probs = 0.75, na.rm=T),
                   unique_resp_q90 = quantile(avg_unique_resp_contacts, probs = 0.90, na.rm=T),
                   unique_resp_mean = mean(avg_unique_resp_contacts, na.rm=T),
                   unique_ent_q75 = quantile(avg_unique_ent_contacts, probs = 0.75, na.rm=T),
                   unique_ent_q90 = quantile(avg_unique_ent_contacts, probs = 0.90, na.rm=T),
                   unique_ent_mean = mean(avg_unique_ent_contacts, na.rm=T),
                   daily_resp_q75 = quantile(avg_daily_resp_contacts, probs = 0.75, na.rm=T),
                   daily_resp_q90 = quantile(avg_daily_resp_contacts, probs = 0.90, na.rm=T),
                   daily_resp_mean = mean(avg_daily_resp_contacts, na.rm=T),
                   daily_ent_q75 = quantile(avg_daily_ent_contacts, probs = 0.75, na.rm=T),
                   daily_ent_q90 = quantile(avg_daily_ent_contacts, probs = 0.90, na.rm=T),
                   daily_ent_mean = mean(avg_daily_ent_contacts, na.rm=T))

contacts_resp_ent <- contacts_resp_ent %>%
  mutate(daily_resp_mean_outlier = 
           ifelse(avg_daily_resp_contacts > contact_summaries$daily_resp_mean, 1, 0),
         daily_resp_q75_outlier = 
           ifelse(avg_daily_resp_contacts > contact_summaries$daily_resp_q75, 1, 0),
         daily_ent_mean_outlier = 
           ifelse(avg_daily_ent_contacts > contact_summaries$daily_ent_mean, 1, 0),
         daily_ent_q75_outlier = 
           ifelse(avg_daily_ent_contacts > contact_summaries$daily_ent_q75, 1, 0),
         unique_resp_mean_outlier = 
           ifelse(avg_unique_resp_contacts > contact_summaries$unique_resp_mean, 1, 0),
         unique_resp_q75_outlier = 
           ifelse(avg_unique_resp_contacts > contact_summaries$unique_resp_q75, 1, 0),
         unique_ent_mean_outlier = 
           ifelse(avg_unique_ent_contacts > contact_summaries$unique_ent_mean, 1, 0),
         unique_ent_q75_outlier = 
           ifelse(avg_unique_ent_contacts > contact_summaries$unique_ent_q75, 1, 0))

saveRDS(contacts_resp_ent, here("outlier-analysis/data/contacts_resp_ent.RDS"))

#Lite analysis for abstract ---------------------------------------------------
#Average/Q75 respiratory and enteric contacts
contact_summaries

#Include number of outliers as well per each definition 
table(contacts_resp_ent$daily_ent_q75_outlier)
336/(336+1024)
table(contacts_resp_ent$daily_resp_q75_outlier)
317/(317+1046)

#Number of outliers by site respiratory
table(contacts_resp_ent$daily_resp_q75_outlier, contacts_resp_ent$study_site)
#Rural: 31%
217/(217+479)
#Urban: 15%
100/(100+567)

#Number of outliers by site enteric
table(contacts_resp_ent$daily_ent_q75_outlier, contacts_resp_ent$study_site)
#Rural: 32%
220/(220+473)
#Urban: 17%
116/(116+551)

# Proportion of contacts that were considered both
table(df_contact$respiratory == 1 & df_contact$enteric == 1)
16221 / (16221 + 3708)

# Proportion of contacts that were considered both by site
table(df_contact$respiratory == 1 & df_contact$enteric == 1, df_contact$study_site)
#Rural: 78%
9155/(9155+2627)
#Urban: 87%
7066/(7066+1081)

#Average respiratory and enteric contacts by rural/urban 
