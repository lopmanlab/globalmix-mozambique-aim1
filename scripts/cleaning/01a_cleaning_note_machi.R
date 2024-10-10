Sys.setenv(LANGUAGE = "en")
Sys.setlocale("LC_ALL", "C")
Sys.setlocale("LC_TIME", "C")

# Line 327 - 337
# Added transport use == 5
participant <- participant %>% 
  dplyr::mutate(transport_use = case_when(transport_use == 0 ~ "Never", 
                                          transport_use == 1 ~ "Rarely", 
                                          transport_use == 2 ~ "Daily/ almost daily",
                                          transport_use == 3 ~ "1-3 times per week", 
                                          transport_use == 4 ~ "1 time per two weeks",
                                          transport_use == 5 ~ "1 time per month"))
participant$transport_use <- factor(participant$transport_use, 
                                    levels = c("Never", "Rarely", "Daily/ almost daily",
                                               "1-3 times per week", "1 time per two weeks","1 time per month"),
                                    labels = c("Never", "Rarely", "Daily",
                                               "≤3 times per week", "Once every two weeks","Once monthly"))

# participant <- participant %>% 
#   dplyr::mutate(transport_use = case_when(transport_use == 0 ~ "Never", 
#                                           transport_use == 1 ~ "Rarely", 
#                                           transport_use == 2 ~ "Daily/ almost daily",
#                                           transport_use == 3 ~ "1-3 times per week", 
#                                           transport_use == 4 ~ "1 time per month"))
# participant$transport_use <- factor(participant$transport_use, 
#                                     levels = c("Never", "Rarely", "Daily/ almost daily",
#                                                "1-3 times per week", "1 time per month"),
#                                     labels = c("Never", "Rarely", "Daily",
#                                                "≤3 times per week", "Once monthly"))

# Run up till the household contacts
# import excel and check the age
age_rural <- read_excel("C:/Users/damac/OneDrive - Emory University/Emory University/GlobalMix Summer Job/MANHICA_RURAL_age_categories_verified.xlsx")
age_urban <- read_excel("C:/Users/damac/OneDrive - Emory University/Emory University/GlobalMix Summer Job/POLANA CANICO_URBAN_age_categories_verified.xlsx.xls")
age_rural <- age_rural%>%
  select(rec_id, hh_member_id, hh_member_name, months)
age_urban <- age_urban%>%
  select(c("rec_id", "hh_member_id", "hh_member_name", "Age(months)"))%>%
  rename(months = "Age(months)")

age_combined <- rbind(age_rural, age_urban)
age_combined <- age_combined%>%
  filter(!duplicated(hh_member_id))

# Extract the age = 0 contacts
contact_age0 <- contact_hh_all%>%
  filter(hh_member_age == 0)

# Combine the dataset
contact_age0_months <- contact_age0%>%
  left_join(age_combined, by = "hh_member_id")%>%
  mutate(rec_id_match = ifelse(rec_id.x == rec_id.y, T, F),
         name_match = ifelse(hh_member_name.x == hh_member_name.y, T, F),
         months = ifelse(rec_id_match == F & name_match == F, NA, months))%>%
  rename(rec_id = rec_id.x,
         hh_member_name = hh_member_name.x)%>%
  select(-c("rec_id.y", "hh_member_name.y", "rec_id_match", "name_match"))


# Line 1504 - 1531
contact_hh_all <- contact_hh_all %>% # 10542
  rename(contact_sex = hh_member_sex) %>%
  dplyr::mutate(contact_age = case_when(as.numeric(hh_member_age) == 0 ~ "<11mo",
                                        as.numeric(hh_member_age) %in% 1:4 ~ "1-4y",
                                        as.numeric(hh_member_age) %in% 5:9 ~ "5-9y",
                                        as.numeric(hh_member_age) %in% 10:14 ~ "10-14y",
                                        as.numeric(hh_member_age) %in% 15:19 ~ "15-19y",
                                        as.numeric(hh_member_age) %in% 20:29 ~ "20-29y",
                                        as.numeric(hh_member_age) %in% 30:39 ~ "30-39y",
                                        as.numeric(hh_member_age) %in% 40:49 ~ "40-59y", # 40-59
                                        as.numeric(hh_member_age) %in% 50:59 ~ "40-59y", # 40-59
                                        as.numeric(hh_member_age) >=60 ~ "60+y")) %>%
  select(-c(had_contact, share_room, share_bed, cook_food, redcap_event_name,
            hh_member_hoh, hh_member_id2, had_contact, share_bed, cook_food,
            contact_count, redcap_event_name, study_site,
            hh_member_age))

# Line 1535 - 1560
contact_nonhh_all <- contact_nonhh_all %>%
  rename(contact_sex = sex_contact) %>%
  dplyr::mutate(contact_age = case_when(age_group_contact == 0 ~ "<6mo",
                                        age_group_contact == 1 ~ "6-11mo",
                                        age_group_contact == 2 ~ "1-4y",
                                        age_group_contact == 3 ~ "5-9y",
                                        age_group_contact == 4 ~ "10-14y",
                                        age_group_contact == 4 ~ "15-19y",
                                        age_group_contact == 5 ~ "20-29y",
                                        age_group_contact == 6 ~ "30-39y",
                                        age_group_contact == 7 ~ "40-59y", # 40-59
                                        age_group_contact == 8 ~ "40-59y", # 40-59
                                        age_group_contact == 9 ~ "60+y",
                                        age_group_contact == 10 ~ "Missing",
                                        as.numeric(age_contact) == 0 ~ "6-11mo",
                                        as.numeric(age_contact) %in% 1:4 ~ "1-4y",
                                        as.numeric(age_contact) %in% 5:9 ~ "5-9y",
                                        as.numeric(age_contact) %in% 10:14 ~ "10-14y",
                                        as.numeric(age_contact) %in% 15:19 ~ "15-19y",
                                        as.numeric(age_contact) %in% 20:29 ~ "20-29y",
                                        as.numeric(age_contact) %in% 30:39 ~ "30-39y",
                                        as.numeric(age_contact) %in% 40:49 ~ "40-59y", # 40-59
                                        as.numeric(age_contact) %in% 50:59 ~ "40-59y", # 40-59
                                        as.numeric(age_contact) >=60 ~ "60+y")) %>%
  select(-c(study_site, contact_count, contact_age_yn, contact_id,
            age_group_contact, age_contact, location_contact_other))

# Household member relationship (line 1694)
 # Maybe it is better to make the non-household member as non-household member 
 # rather than putting everyone to unknown


# Data sharing preparation

# # For Khang
participants_aim1_sel <- participants_aim1%>%
  select(rec_id, study_site, aim, date_enrolled, participant_age, age, months, participant_sex, enrolled_school, 
         school_level, read_write, highest_education, occupation, transport_use, hh_resp_exposure, hh_size)%>%
  rename(study_aim = aim,
         date_participant_enrolled = date_enrolled)
#saveRDS(participants_aim1_sel, "C:/Users/damac/OneDrive - Emory University/3.Global Mix/Globalmix clean data/Mozambique/aim_1/moz_participant_data_aim1_age.RDS")


# For Ani
participants_aim1_ani <- participants_aim1%>%
  select(rec_id, study_site, participant_age, age, months, participant_sex, occupation, transport_use, mask_use)
#saveRDS(participants_aim1_ani, "C:/Users/damac/OneDrive - Emory University/3.Global Mix/Globalmix clean data/Mozambique/aim_1/moz_participant_data_aim1_ani.RDS")

# Run up till before filtering out had_contact = "Yes"
# Then, run this code
house_age_ani <- contact_hh_all%>%
  filter(!duplicated(hh_member_id2))%>%
  mutate(hh_member_age = as.numeric(hh_member_age),
         hh_member_age_group = case_when(as.numeric(hh_member_age) == 0 ~ "6-11mo",
                                         as.numeric(hh_member_age) %in% 1:4 ~ "1-4y",
                                         as.numeric(hh_member_age) %in% 5:9 ~ "5-9y",
                                         as.numeric(hh_member_age) %in% 10:19 ~ "10-19y",
                                         as.numeric(hh_member_age) %in% 20:29 ~ "20-29y",
                                         as.numeric(hh_member_age) %in% 30:39 ~ "30-39y",
                                         as.numeric(hh_member_age) %in% 40:49 ~ "40-59y", # 40-59
                                         as.numeric(hh_member_age) %in% 50:59 ~ "40-59y", # 40-59
                                         as.numeric(hh_member_age) >=60 ~ "60+y"))%>%
  filter(aim == 0|aim == 1 & isindex == 1)%>%
  select(rec_id, hh_member_id2, survey_date, hh_member_age, hh_member_sex, hh_member_relationship, hh_member_relationship_other, study_site, aim, isindex, hh_member_age_group)
  # select(-c(redcap_repeat_instrument, redcap_repeat_instance, redcap_event_name, 
  #           hh_member_name, hh_member_hoh, share_room, share_bed, cook_food, 
  #           informaes_de_agregado_familiar_e_casa_complete, contact_count, repeat_contact_d2, contact_id, hh_member_id, study_day))


# Not sure if this is necessary, but if so, run the contact_non_hh_all code and then, run this code
house_age_ani2 <- contact_nonhh_all%>%
  filter(hh_membership == "Member")%>%
  mutate(hh_member_sex = sex_contact,
         hh_member_age = as.numeric(age_contact),
         hh_member_age_group = case_when(as.numeric(age_contact) == 0 ~ "6-11mo",
                                         as.numeric(age_contact) %in% 1:4 ~ "1-4y",
                                         as.numeric(age_contact) %in% 5:9 ~ "5-9y",
                                         as.numeric(age_contact) %in% 10:14 ~ "10-19y",
                                         as.numeric(age_contact) %in% 20:29 ~ "20-29y",
                                         as.numeric(age_contact) %in% 30:39 ~ "30-39y",
                                         as.numeric(age_contact) %in% 40:49 ~ "40-59y", # 40-59
                                         as.numeric(age_contact) %in% 50:59 ~ "40-59y", # 40-59
                                         as.numeric(age_contact) >=60 ~ "60+y",
                                         age_group_contact == 0 ~ "<6mo",
                                         age_group_contact == 1 ~ "6-11mo",
                                         age_group_contact == 2 ~ "1-4y",
                                         age_group_contact == 3 ~ "5-9y",
                                         age_group_contact == 4 ~ "10-19y",
                                         age_group_contact == 5 ~ "20-29y",
                                         age_group_contact == 6 ~ "30-39y",
                                         age_group_contact == 7 ~ "40-59y", # 40-59
                                         age_group_contact == 8 ~ "40-59y", # 40-59
                                         age_group_contact == 9 ~ "60+y",
                                         age_group_contact == 10 ~ NA))%>%
  select(rec_id, survey_date, hh_member_age, hh_member_sex, study_site, aim, isindex, fromdayone, hh_member_age_group)
 
house_age_ani$hh_member_age_group <- factor(house_age_ani$hh_member_age_group, 
                                      levels = c("<6mo", "6-11mo", "1-4y", "5-9y", 
                                                 "10-19y", "20-29y", "30-39y", 
                                                 "40-59y", "60+y")) 
house_age_ani2$hh_member_age_group <- factor(house_age_ani2$hh_member_age_group, 
                                       levels = c("<6mo", "6-11mo", "1-4y", "5-9y", 
                                                  "10-19y", "20-29y", "30-39y", 
                                                  "40-59y", "60+y"))

house_age_ani_comp <- house_age_ani%>%
  # bind_rows(house_age_ani2)%>% #combine if necessary
  mutate(hh_member_sex = case_when(hh_member_sex == 0 ~ "Female",
                                 hh_member_sex == 1 ~ "Male",
                                 TRUE ~ "Other"),
         hh_member_relationship = case_when(hh_member_relationship == 0 ~ "Spouse",
                                            hh_member_relationship == 1 ~ "Sibling",
                                            hh_member_relationship == 2 ~ "Child",
                                            hh_member_relationship == 3 ~ "Parent",
                                            hh_member_relationship == 4 ~ "Grandparent",
                                            hh_member_relationship == 5 ~ "Uncle/Aunt",
                                            hh_member_relationship == 6 ~ "Grandchild",
                                            hh_member_relationship == 7 ~ "Nephew",
                                            hh_member_relationship == 8 ~ "Other relative",
                                            hh_member_relationship == 9 ~ "Not related",
                                            hh_member_relationship == 10 ~ "Self",
                                            is.na(hh_member_relationship) ~ "Unknown"))%>%
  select(-c(hh_member_relationship_other, isindex))

#saveRDS(house_age_ani_comp, "C:/Users/damac/OneDrive - Emory University/3.Global Mix/Globalmix clean data/Mozambique/aim_1/moz_hh_age_comp.RDS")


## hh_size try out
hh_id <- data%>%
  filter(!is.na(hh_id))%>%
  select(rec_id, hh_id, hh_occupants, redcap_repeat_instrument, redcap_event_name)%>%
  distinct(rec_id, hh_id, .keep_all = TRUE)

participants_aim1_ext <- participants_aim1%>%
  select(rec_id, hh_size)%>%
  full_join(hh_id, by = "rec_id")%>%
  mutate(hh_occupants = as.numeric(hh_occupants))

table(participants_aim1_ext$hh_size, participants_aim1_ext$hh_occupants, useNA = "a")
## Seems like it is not possible to get the household size for those missing