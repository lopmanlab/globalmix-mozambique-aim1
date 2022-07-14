# extract aim 2 participant details

rm(list=ls())
source("./scripts/00_load_libraries.R")

individual_info <- readRDS("./Data/individual_info.RDS")
aim2 <- individual_info %>% filter(aim==2)

keep_vars <- c("rec_id","aim","sensor_id", "sex", "age_cat",         
                "enrolled_school","highest_educ","school_level",            
                "transport_use","hh_resp_exposure","child_breastfeed",        
                "num_sibling","child", "study_site")

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
                       "6853","1600","1004") # 1700

aim2_rural$sensor_id[aim2_rural$sensor_id=="63DD440BD"] <- "63DD40BD"
aim2_rural <- subset(aim2_rural, !(rec_id %in% drop_sensor_rural)) 

write.csv(aim2_rural,"./Data/mozambique_aim2_rural_participants.csv" )



aim2_urban <- aim2 %>%
  filter(study_site=="Polana Caniço")
