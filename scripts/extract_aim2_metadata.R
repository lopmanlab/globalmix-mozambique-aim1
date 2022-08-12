# extract aim 2 participant details

rm(list=ls())
library(tidyr)

# source("./scripts/00_load_libraries.R")

aim2 <- readRDS("./Data/individual_info.RDS") %>% 
  filter(aim==2)

# names(aim2)
# table(aim2$study_site) # 418 rural, 367 urban

keep_vars <- c("rec_id","aim","sensor_id", "sex", "age_cat",         
                "enrolled_school","highest_educ","school_level",            
                "transport_use","hh_resp_exposure","child_breastfeed",        
                "num_sibling","child", "study_site")


# Rural site
aim2_rural <- aim2 %>%
  filter(study_site=="Manhiça") %>%
  select(all_of(keep_vars))

rural_summary <- aim2_rural %>%
  select("sex", "age_cat",         
         "enrolled_school","highest_educ","school_level",            
         "transport_use","hh_resp_exposure","child_breastfeed",        
         "num_sibling","child") %>%
  tbl_summary(percent="column",
              digits = all_categorical() ~ 1) %>%
  bold_labels() %>%
  modify_header(label = "**Variable**") %>%
  modify_caption("**Particiant Individual Information in Manhiça**")
rural_summary

# check sensor ids
# currently, check for these rec_ids: 
drop_sensor_rural <- c("1263","478","942","867","1378",
                       "6853","1600","1004", "531", "1463")
aim2_rural <- subset(aim2_rural, !(rec_id %in% drop_sensor_rural)) 
aim2_rural$sensor_id[aim2_rural$sensor_id=="63DD440BD"] <- "63DD40BD"

write.csv(aim2_rural,"./data/mozambique_aim2_rural_participants.csv" )

# save unique sensor IDs used by participants
rural_unique_sensors <- aim2_rural %>% 
  select(rec_id, sensor_id, study_site) %>%
  distinct(sensor_id, .keep_all = TRUE)

write.csv(rural_unique_sensors, "./data/mozambique_rural_unique_sensors.csv")


# Urban site
# table(aim2$study_site)
aim2_urban <- aim2 %>%
  filter(study_site == "Polana Caniço") %>%
  select(all_of(keep_vars))

urban_summary <- aim2_urban %>%
  select("sex", "age_cat",         
         "enrolled_school","highest_educ","school_level",            
         "transport_use","hh_resp_exposure","child_breastfeed",        
         "num_sibling","child") %>%
  tbl_summary(percent="column",
              digits = all_categorical() ~ 1) %>%
  bold_labels() %>%
  modify_header(label = "**Variable**") %>%
  modify_caption("**Particiant Individual Information in Manhiça**")
urban_summary

########## check sensor ids
# there are some with values of 0,2,A, some NAs, some incorrectly captured.
# make a list of rec_id and drop
urban_incorrect_sensor_ids <- aim2_urban %>%
  select(rec_id, sensor_id, study_site) %>%
  filter(is.na(sensor_id) | sensor_id=="0" | sensor_id=="2" |
           sensor_id=="A" | rec_id=="736" | rec_id=="610" | rec_id=="1746" |
           rec_id=="1955" | rec_id=="1530")
write.csv(urban_incorrect_sensor_ids, "./data/urban_incorrect_sensor_id.csv")

# making corrections to sensor IDs
aim2_urban$sensor_id[aim2_urban$sensor_id=="019664B1A"] <- "01964B1A"
aim2_urban$sensor_id[aim2_urban$sensor_id=="6CED070"] <- "6CECD070"
aim2_urban$sensor_id[aim2_urban$sensor_id=="90CBF5950"] <- "90CBF595"
aim2_urban$sensor_id[aim2_urban$sensor_id=="AD C6D682"] <- "ADC6D682"
aim2_urban$sensor_id[aim2_urban$sensor_id=="FDF9728"] <- "FDF97280"

# drop these rec_ids for now, but confirm correct sensor_id
drop_sensor_urban <- c("736", "610", "1746", "1955",
                       urban_incorrect_sensor_ids$rec_id)
aim2_urban <- subset(aim2_urban, !(rec_id %in% drop_sensor_urban))

write.csv(aim2_urban,"./data/mozambique_aim2_urban_participants.csv" )

# save unique sensor IDs used by participants
urban_unique_sensors <- aim2_urban %>% 
  select(rec_id, sensor_id, study_site) %>%
  distinct(sensor_id, .keep_all = TRUE)

write.csv(urban_unique_sensors, "./data/mozambique_urban_unique_sensors.csv")
