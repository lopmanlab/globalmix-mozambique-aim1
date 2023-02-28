# extract aim 2 participant details for use in sensor analysis.

rm(list=ls())
pacman::p_load(tidyr, dplyr, gtsummary)

aim2 <- readRDS("./data/clean/participant_data_aim2.RDS") %>%
  select("rec_id", "isindex", "aim", "study_site", "sensor_id", 
         "participant_sex", "sex_other", "age", "participant_age", "read_write", 
         "occupation", "occupation_other", "enrolled_school", "highest_educ",  
         "school_level", "transport_use", "hh_resp_exposure", "child", 
         "child_breastfeed", "num_sibling")

table(aim2$study_site) # 418 rural, 367 urban


# Rural site
aim2_rural <- aim2 %>%
  filter(study_site=="Rural")

rural_summary_all <- aim2_rural %>%
  tbl_summary(percent="column",
              digits = all_categorical() ~ 1) %>%
  bold_labels() %>%
  modify_header(label = "**Variable**") %>%
  modify_caption("**Particiant information in Manhiça**")
rural_summary_all

# check sensor ids
table()
# currently, check for these rec_ids: 
drop_sensor_rural <- c("1263","478","942","867","1378",
                       "6853","1600","1004", "531", "1463")

aim2_rural <- subset(aim2_rural, !(rec_id %in% drop_sensor_rural))

aim2_rural$sensor_id[aim2_rural$sensor_id=="63DD440BD"] <- "63DD40BD"
aim2_rural$sensor_id[aim2_rural$sensor_id=="OC0516EA"] <- "0C0516EA"

write.csv(drop_sensor_rural, "./data/sensor_metadata/rural_incorrect_sensor_id.csv")

# there are 408 (-7 NAs) individuals who carried sensors.

## merge with rural sensor metadata
rural_meta <- read.csv("./data/sensor_metadata/mozambique_rural_sensor_meta.csv")

aim2_rural_final <- aim2_rural %>%
  inner_join(rural_meta, by="sensor_id") %>%
  select("rec_id", "study_site", "aim", "sensor_id", "hwid", "sex", "age_cat", 
         "enrolled_school", "highest_educ", "school_level") %>%
  mutate(study_site = case_when(study_site=="Manhiça" ~ "Manhica")) %>%
  filter(.$hwid != 0)

# there are duplicates by rec_id
rural_duplicates <- aim2_rural_final$rec_id[duplicated(aim2_rural_final$rec_id)]
aim2_rural_final <- aim2_rural_final %>%
  distinct(rec_id, .keep_all = TRUE)

rural_summary_final <- aim2_rural_final %>%
  select("sex", "age_cat", "enrolled_school", "highest_educ", "school_level") %>%
  tbl_summary(percent="column",
              digits = all_categorical() ~ 1) %>%
  bold_labels() %>%
  modify_header(label = "**Variable**") %>%
  modify_caption("**Particiant information in Manhiça (all records ok)**")
rural_summary_final

# out of 408, we can only analyze 285 (70%) records since these have matching hwids.
# 5 damaged sensors + 123 records dropped due to missing hwid.
# 
write.csv(aim2_rural_final,"./data/sensor_metadata/mozambique_aim2_rural_participants.csv" )

# check characteristics of dropped sensors
aim2_rural_drop <- aim2_rural %>%
  left_join(rural_meta, by="sensor_id") %>%
  filter(is.na(hwid) | hwid==0)

rural_summary_drop <- aim2_rural_drop %>%
  select("sex", "age_cat", "enrolled_school", "highest_educ", "school_level") %>%
  tbl_summary(percent="column",
              digits = all_categorical() ~ 0) %>%
  bold_labels() %>%
  modify_header(label = "**Variable**") %>%
  modify_caption("**Dropped participants in Manhiça**")
rural_summary_drop

# save unique sensor IDs used by rural participants
rural_unique_sensors <- aim2_rural_final %>% 
  select(sensor_id, study_site) %>%
  distinct(sensor_id, .keep_all = TRUE)

write.csv(rural_unique_sensors, "./data/sensor_metadata/mozambique_rural_unique_sensors.csv")


# Urban site
aim2_urban <- aim2 %>%
  filter(study_site=="Polana Caniço") %>%
  select(all_of(keep_vars))

urban_summary_all <- aim2_urban %>%
  select("sex", "age_cat", "enrolled_school", "highest_educ", "school_level") %>%
  tbl_summary(percent="column",
              digits = all_categorical() ~ 0) %>%
  bold_labels() %>%
  modify_header(label = "**Variable**") %>%
  modify_caption("**Particiant information in Manhiça**")
urban_summary_all

########## check sensor ids
# there are some with values of 0,2,A, some NAs, some incorrectly captured.
# make a list of rec_id and drop
drop_sensor_urban <- aim2_urban %>%
  select(rec_id, sensor_id, study_site) %>%
  filter(is.na(sensor_id) | sensor_id=="0" | sensor_id=="2" | sensor_id=="A")
nrow(drop_sensor_urban) # 56

# drop these 56 rec_ids for now, but confirm correct sensor_id
aim2_urban <- aim2_urban %>%
  subset(., !(rec_id %in% drop_sensor_urban$rec_id)) 

write.csv(drop_sensor_urban, "./data/sensor_metadata/urban_incorrect_sensor_id.csv")

# making corrections to sensor IDs
aim2_urban$sensor_id[aim2_urban$sensor_id=="019664B1A"] <- "01964B1A"
aim2_urban$sensor_id[aim2_urban$sensor_id=="6CED070"] <- "6CECD070"
aim2_urban$sensor_id[aim2_urban$sensor_id=="90CBF5950"] <- "90CBF595"
aim2_urban$sensor_id[aim2_urban$sensor_id=="AD C6D682"] <- "ADC6D682"
aim2_urban$sensor_id[aim2_urban$sensor_id=="FDF9728"] <- "FDF97280"
aim2_urban$sensor_id[aim2_urban$sensor_id=="30BEADD1"] <- "30DEADD1"
aim2_urban$sensor_id[aim2_urban$sensor_id=="67FDE60EB"] <- "67FD60EB"

# delete duplicates
aim2_urban <- aim2_urban %>%
  distinct(rec_id, .keep_all = TRUE)

## merge with urban sensor metadata
urban_meta <- read.csv("./data/sensor_metadata/mozambique_urban_sensor_meta.csv")

# check for duplicates in rec_id

aim2_urban_final <- aim2_urban %>%
  inner_join(urban_meta, by="sensor_id") %>%
  select("rec_id", "study_site", "aim", "sensor_id", "hwid", "sex", "age_cat", 
         "enrolled_school", "highest_educ", "school_level") %>%
  mutate(study_site = case_when(study_site=="Polana Caniço" ~ "Polana Canico")) %>%
  filter(!is.na(hwid)) # drop na hwid, lost sensors

# there are duplicates by rec_id
urban_duplicates <- aim2_urban_final$rec_id[duplicated(aim2_urban_final$rec_id)]
aim2_urban_final <- aim2_urban_final %>%
  distinct(rec_id, .keep_all = TRUE)

urban_summary_final <- aim2_urban_final %>%
  select("sex", "age_cat", "enrolled_school", "highest_educ", "school_level") %>%
  tbl_summary(percent="column",
              digits = all_categorical() ~ 0) %>%
  bold_labels() %>%
  modify_header(label = "**Variable**") %>%
  modify_caption("**Particiant information in Manhiça (all records ok)**")
urban_summary_final
# out of 367, we can only analyze 268 (73%) records since these have matching hwids.
# 56 records dropped due to missing hwid.
write.csv(aim2_urban_final,"./data/sensor_metadata/mozambique_aim2_urban_participants.csv" )

# check characterisrics of dropped sensors
aim2_urban_drop <- aim2_urban %>%
  left_join(urban_meta, by="sensor_id") %>%
  filter(is.na(hwid))

urban_summary_drop <- aim2_urban_drop %>%
  select("sex", "age_cat", "enrolled_school", "highest_educ", "school_level") %>%
  tbl_summary(percent="column",
              digits = all_categorical() ~ 0) %>%
  bold_labels() %>%
  modify_header(label = "**Variable**") %>%
  modify_caption("**Dropped participants in P. C.**")
urban_summary_drop

# save unique sensor IDs used by urban participants
urban_unique_sensors <- aim2_urban_final %>% 
  select(sensor_id, study_site) %>%
  distinct(sensor_id, .keep_all = TRUE)

write.csv(urban_unique_sensors, "./data/sensor_metadata/mozambique_urban_unique_sensors.csv")

rm(list=ls())
