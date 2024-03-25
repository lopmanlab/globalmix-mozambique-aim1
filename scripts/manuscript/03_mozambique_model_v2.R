###############################################################################  
# This file contains scripts to analyze and visualize place use data.
# Author: Sara Kim
# Date: 08/18/2023; 
# modified 03/12/2024 - included updated Globalmix matrices and 2021 Prem et al matrices
############################################################################### 

##### Data cleaning for socialmixr package
contacts2 <- df_contact_d1 %>%
  # contacts %>%
  # left_join(participants %>% 
  #             select(rec_id, "participant_age"), 
  #           by = c("rec_id"="rec_id")) %>%
  mutate(part_age = participant_age,
         part_id = rec_id) 


contacts2 <- contacts2 %>%
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

##### Weighting
### age categories to join with population weighting
contacts2 <- contacts2 %>%
  mutate(
   part_age = as.character(part_age),
   weight_cat = case_when(
      part_age == "<6mo"  ~ "0-9y",
      part_age == "6-11mo" ~ "0-9y",
      part_age == "1-4y" ~ "0-9y",
      part_age == "5-9y" ~ "0-9y",
      part_age == "10-14y" ~ "10-19y",
      part_age == "15-19y" ~ "10-19y",
      part_age == "20-29y" ~ "20-29y",
      part_age == "30-39y" ~ "30-39y",
      part_age == "40-59y" ~ "40-59y",
      part_age == "60+y" ~ "60+y",
      TRUE ~ NA_character_
    )
  )

### read in population distribution
pop_dist <- rio::import("../../data/clean/moz_pop_dist_new.csv") %>%
  pivot_longer(cols=urban:rural, names_to = "urb_rur", values_to = "tot_pop") %>%
  mutate(study_site = case_when(
    urb_rur =="urban" ~"Urban",
    urb_rur == "rural"~"Rural"
  ))

### weight cleaning
pop_weight <- contacts2 %>%
  select(part_id, study_site, weight_cat) %>% 
  unique() %>%
  group_by(study_site, weight_cat) %>%
  summarise(n=n())
urb_pop <- sum(pop_dist$tot_pop[which(pop_dist$urb_rur=="urban")])
rur_pop <- sum(pop_dist$tot_pop[which(pop_dist$urb_rur=="rural")])

pop_weight <- pop_weight %>% 
  left_join(pop_dist, by = c("weight_cat"="age_cat","study_site"="study_site")) %>%
  left_join(pop_weight %>% 
              group_by(study_site) %>% 
              summarise(tot_site = sum(n)), by = c("study_site"="study_site")) %>%
  mutate(pop_urb_rur =ifelse(study_site=="Rural", rur_pop, urb_pop),
         part_weight = (tot_pop/pop_urb_rur)/(n/tot_site))

### join weights into participants data
contacts2 <- contacts2 %>%
  left_join(pop_weight %>% select(study_site, weight_cat, part_weight),
            by = c("study_site"="study_site", "weight_cat"="weight_cat"))


##### Create survey structures for input into socialmixr package
contacts2 <- contacts2 %>% 
  mutate(
         part_age = as.character(part_age),
         contact_age = as.character(contact_age),
         cnt_age_est_min = case_when( 
           ## If dont have exact contact age, need to specify a minimum and maximum based on the age group range
           contact_age == "<6mo" ~0,
           contact_age == "6-11mo"~0,
           contact_age == "1-4y" ~0,
           contact_age == "5-9y" ~ 0,
           contact_age == "10-14y"~10,
           contact_age == "15-19y"~10,
           contact_age == "20-29y"~20,
           contact_age == "30-39y"~30,
           contact_age == "40-59y"~40,
           contact_age == "60+y"~60,
           contact_age== "I don't know" ~ NA_real_,
           is.na(contact_age) ~ NA_real_
         ),
         
         cnt_age_est_max= case_when(
           contact_age == "<6mo" ~9,
           contact_age == "6-11mo"~9,
           contact_age == "1-4y" ~ 9,
           contact_age == "5-9y" ~ 9,
           contact_age == "10-14y"~19,
           contact_age == "15-19y"~19,
           contact_age == "20-29y"~29,
           contact_age == "30-39y"~39,
           contact_age == "40-59y"~59,
           contact_age == "60+y"~99,
           contact_age== "I don't know" ~ NA_real_,
           is.na(contact_age) ~ NA_real_
         ),
         
         part_age_est_min = case_when(
           part_age == "<6mo" ~0,
           part_age == "6-11mo"~0,
           part_age == "1-4y" ~0,
           part_age == "5-9y" ~ 0,
           part_age == "10-14y"~10,
           part_age == "15-19y"~10,
           part_age == "20-29y"~20,
           part_age == "30-39y"~30,
           part_age == "40-59y"~40,
           part_age == "60+y"~60,
           part_age== "I don't know" ~ NA_real_,
           is.na(part_age) ~ NA_real_
         ),
         
         part_age_est_max= case_when(
           part_age == "<6mo" ~9,
           part_age == "6-11mo"~9,
           part_age == "1-4y" ~ 9,
           part_age == "5-9y" ~ 9,
           part_age == "10-14y"~19,
           part_age == "15-19y"~19,
           part_age == "20-29y"~29,
           part_age == "30-39y"~39,
           part_age == "40-59y"~59,
           part_age == "60+y"~99,
           part_age== "I don't know" ~ NA_real_,
           is.na(part_age) ~ NA_real_
         ),
  ) 

### One urban and one rural
cnt_u <- contacts2 %>% 
  filter(study_site == "Urban")

cnt_r <- contacts2 %>% 
  filter(study_site == "Rural")


df_r <- survey(
  participants = cnt_r %>% 
    select(part_id, part_age, part_age_est_min, part_age_est_max, part_weight) %>% 
    unique() %>%
    rename(weights = part_weight) %>%
    mutate(country = rep("Mozambique"),
           year = rep(2020)),
  
  contacts = cnt_r %>%
    select(part_id, cnt_age_est_min, cnt_age_est_max, cnt_home:cnt_otherplace)
  
)

df_u <- survey(
  participants = cnt_u %>% 
    select(part_id, part_age, part_age_est_min, part_age_est_max, part_weight) %>% 
    unique()%>%
    rename(weights = part_weight) %>%
    mutate(country=rep("Mozambique"),
           year=rep(2020)),
  
  contacts = cnt_u %>%
    select(part_id,cnt_age_est_min, cnt_age_est_max, cnt_home:cnt_otherplace)
  
)


##### Creating matrices

### urban
## EDIT 06/02: Age groups changed from 40-59 to 40-49 and 50-59
m_u <- contact_matrix(df_u, 
                      age.limits = c(0,10,20,30,40,50,60), ##Specify age bands for the matrix
                      symmetric=T,  ##symmetric matrix
                      estimated.participant.age = "sample",
                      missing.contact.age="sample",
                      # missing.participant.age="remove",
                      estimated.contact.age = "sample",  
                      ##what to do if missing contact age group, use sample with bootstraping
                      weigh.age = T,
                      return.part.weights=T,
                      n=1000)     ##Number of bootstraps


#Code to take the mean of matrices
mr_u <- Reduce("+", lapply(m_u$matrices, function(x) {x$matrix})) / length(m_u$matrices)

#Make into long form
mat_u <- reshape2::melt(mr_u, varnames = c("age1", "age_cont"), value.name = "contacts") %>%
  left_join(data.frame(age1 = c(1,2,3,4,5,6,7),
                       age_part = c("0-9y","10-19y","20-29y","30-39y","40-49y", "50-59y","60+y")),
            by="age1")

### Do same for rural
m_r <- contact_matrix(df_r, age.limits = c(0,10,20,30,40,50,60),
                      symmetric=T, 
                      estimated.participant.age = "sample",
                      missing.contact.age="sample",
                      estimated.contact.age = "sample",
                      return.part.weights = T,
                      n=1000)


mr_r <- Reduce("+", lapply(m_r$matrices, function(x) {x$matrix})) / length(m_r$matrices)
mat_r <- reshape2::melt(mr_r, varnames = c("age1", "age_cont"), value.name = "contacts") %>%
  # EDIT 06/02: Age groups changed from 40-59 to 40-49 and 50-59
  left_join(data.frame(age1 = c(1,2,3,4,5,6,7),
                       age_part = c("0-9y","10-19y","20-29y","30-39y","40-49y","50-59y","60+y")),
            by="age1")


##### Matrix visualization
### rural
## EDIT 06/02: Age groups changed from 40-59 to 40-49 and 50-59

ruralmatrix <- mat_r %>%
  mutate(age_cont = case_when(age_cont == "[0,10)" ~ "0-9y", 
                              age_cont == "[10,20)" ~ "10-19y", 
                              age_cont == "[20,30)" ~ "20-29y",
                              age_cont == "[30,40)" ~ "30-39y", 
                              age_cont == "[40,50)" ~  "40-49y",
                              age_cont == "[50,60)" ~  "50-59y",
                              age_cont == "60+" ~ "60+y")) %>%
  ggplot(aes(x = age_part, y = age_cont, fill = contacts)) + 
  geom_tile() + 
  scale_fill_gradient2(low="#91bfdb", mid="#fee090", high="#d73027", 
                       midpoint = 4, limit = c(0,8)) +
  labs(x = "Participant age",
       y = "Contact age",
       title = "Rural") +
  geom_text(aes(label=round(contacts, digits=1)), 
            colour = "black", check_overlap = TRUE, size=2) +
  axis_text_theme2 +
  theme(axis.text.x = element_text(size = 10, angle=90, hjust = 1),,
        legend.position = "top",
        legend.direction  = "horizontal")
ruralmatrix

### urban
## EDIT 06/02: Age groups changed from 40-59 to 40-49 and 50-59
urbanmatrix <- mat_u %>%
  mutate(age_cont = case_when(age_cont == "[0,10)" ~ "0-9y", 
                              age_cont == "[10,20)" ~ "10-19y", 
                              age_cont == "[20,30)" ~ "20-29y",
                              age_cont == "[30,40)" ~ "30-39y",
                              age_cont == "[40,50)" ~ "40-49y",
                              age_cont == "[50,60)" ~ "50-59y",
                              age_cont == "60+" ~ "60+y")) %>%
  ggplot(aes(x = age_part, y = age_cont, fill = contacts)) +
  geom_tile() +
  scale_fill_gradient2(low="#91bfdb", mid="#fee090", high="#d73027", 
                       midpoint = 4, limit = c(0,8))+
  labs(x = "Participant age",
       y = "Contact age",
       title = "Urban") +
  geom_text(aes(label=round(contacts, digits=1)), colour = "black", 
            check_overlap = TRUE, size=2) +
  axis_text_theme2 +
  theme(axis.text.x = element_text(size = 10, angle=90, hjust = 1),,
        legend.position = "top",
        legend.direction  = "horizontal")
# urbanmatrix 


# generate prem matrix
# source("scripts/manuscript/03b_mozambique_prem_matrix.R")
### read in social contact patterns
# default is by 5 year age groups
# updated to 2021 contacts
moz_prem <- read.csv("../../data/clean/moz_prem_2021.csv")

### Generate data frame of participant age groups for orig prem matrix matched to age groups for target matrix
### # EDIT 06/02: Age groups changed from 40-59 to 40-49 and 50-59
age_cat_part <- data.frame(part_age1=c("0_4","5_9","10_14","15_19","20_24","25_29",
                                       "30_34","35_39", "40_44","45_49","50_54",
                                       "55_59","60_64","65_69","70_74","75"),
                           
                           part_age2 =c("0-9y", "0-9y", "10-19y","10-19y", 
                                        "20-29y", "20-29y", "30-39y", "30-39y", 
                                        "40-49y","40-49y", "50-59y", "50-59y",  
                                        "60+y", "60+y", "60+y", "60+y"))


### Generate data frame of contact age groups for orig prem matrix matched to contact age groups for target matrix
# EDIT 06/02: Age groups changed from 40-59 to 40-49 and 50-59
age_cat_cont <- data.frame(cont_age1 = colnames(moz_prem),
                           cont_age2 = c("0-9y", "0-9y", "10-19y","10-19y", 
                                         "20-29y", "20-29y", "30-39y", "30-39y", 
                                         "40-49y","40-49y", "50-59y", "50-59y",  
                                         "60+y", "60+y", "60+y", "60+y"))

moz_prem$part_age1 <- age_cat_part$part_age1


### pivot_longer for ease of data manipulation
moz_prem <- moz_prem %>%
  pivot_longer(cols = contact_0_4:contact_75, 
               names_to = "cont_age",
               values_to = "avg_cont")

### Create a column for the age groups of the target matrix
moz_prem <- moz_prem %>%
  left_join(age_cat_part, by = c("part_age1" = "part_age1")) %>%
  left_join(age_cat_cont, by = c("cont_age" = "cont_age1"))


### population data for 5-year population distribution in Moz
moz_pop_5yr <- read.csv("../../data/clean/agecat_5_total.csv")

### population data for 10-year population distribution in Moz
moz_pop_10yr <- read.csv("../../data/clean/agecat_10_total.csv") %>%
  # recategorize ages to conform to original agecat_10_total part_age2 groups
  mutate(part_age2 = case_when(part_age2 == "0_9" ~ "0-9y", 
                               part_age2 == "10_19" ~ "10-19y", 
                               part_age2 == "20_29" ~ "20-29y",
                               part_age2 == "30_39" ~ "30-39y", 
                               part_age2 == "40_49" ~  "40-49y",
                               part_age2 == "50_59" ~  "50-59y",
                               part_age2 == "60" ~ "60+y")) %>%
  group_by(part_age2) %>%
  summarize(pop10yr = sum(pop10yr))


### "linearly" collapse the participant-contact cells based on population sizes
moz_prem10 <- moz_prem %>%
  left_join(moz_pop_5yr, by = c("part_age1" = "part_age1")) %>%  ## pop size of 5-year participant age band
  mutate(tot_pop_contacts = avg_cont * pop5yr) %>%   ## total contacts made between 0_4 & 0_4, 0_4&5_9 etc
  group_by(part_age2, cont_age2) %>%
  summarise(tot_pop_contacts = sum(tot_pop_contacts)) %>% ##total contacts made between 0_9&0_9, 0_9&10_19 etc
  left_join(moz_pop_10yr, by = c("part_age2"="part_age2")) %>% # pop size of 10-year participant age band
  mutate(contacts = tot_pop_contacts/pop10yr)        ## average contacts made using 10-year age band

### plot
premmatrix <- ggplot(moz_prem10, aes(x = part_age2, y = cont_age2, fill = contacts)) + 
  geom_tile() +
  scale_fill_gradient2(low = "#91bfdb", mid="#fee090", high="#d73027",
                       midpoint = 10, limit = c(0,20))+
  labs(x = "Participant age",
       y = "Contact age",
       title = "Prem et al.") +
  geom_text(aes(label=round(contacts, digits=1)), 
            colour = "black", check_overlap = TRUE, size=2) +
  axis_text_theme2 +
  theme(axis.text.x = element_text(size = 10, angle=90, hjust = 1),
        legend.position = "top",
        legend.direction  = "horizontal")
# premmatrix
# axis.text.y = element_blank(),
# axis.title.y = element_blank()

# combine the matrices
adjusted_matrix <- ruralmatrix | urbanmatrix | premmatrix
adjusted_matrix
ggsave(adjusted_matrix, filename = "../../output/figs/fig_adjusted_matrix.pdf",
       height=8, width=8, dpi=300,
       bg="#FFFFFF")


##### CODE FOR TRANSMISSION MODEL #####
### Fix R0. 2.5 to get q parameter

# function
getr0 <- function(q, CM, d){
  m_ngm <- CM * q * d
  r0 <- max(eigen(as.matrix(m_ngm))$values)
  print(r0)
}

# globalmix rural matrix values
rural <- c(2.98, 2.72, 1.47, 1.69, 1.37, 2.15, 1.49,
           2.00, 7.47, 2.89, 2.34, 2.40, 2.77, 1.84,
           0.74, 1.98, 1.77, 1.42, 1.26, 1.61, 0.99,
           0.59, 1.10, 0.98, 2.12, 1.71, 2.07, 1.47,
           0.30, 0.72, 0.55, 1.09, 1.70, 1.71, 1.22,
           0.31, 0.54, 0.46, 0.86, 1.12, 1.69, 0.91,
           0.22, 0.37, 0.30, 0.63, 0.83, 0.95, 0.99)

cm_rural <- matrix(rural, nrow = 7, ncol = 7)
getr0(q=0.03323546, CM=cm_rural, d=7)

# globalmix urban matrix
urban <- c(1.79, 1.49, 1.38, 1.64, 0.84, 1.23, 0.98,
           1.10, 5.37, 1.90, 2.05, 1.79, 1.81, 1.23,
           0.69, 1.30, 1.09, 1.23, 1.18, 1.13, 0.72,
           0.57, 0.97, 0.85, 1.84, 1.06, 1.24, 0.88,
           0.19, 0.64, 0.52, 0.67, 1.07, 1.12, 0.64,
           0.18, 0.35, 0.32, 0.51, 0.73, 0.68, 0.56,
           0.15, 0.25, 0.22, 0.38, 0.43, 0.58, 0.93)
cm_urban <- matrix(urban, nrow = 7, ncol = 7)
getr0(q=0.04852599, CM=cm_urban, d=7)

# premmatrix

# 2021 values 
prem <- c(7.5, 3.4, 2.2, 3.5, 2.4, 2.1, 1.8,
          2.5, 11.2, 3.1, 2.3, 2.8, 2.2, 1.5,
          1.7, 1.7, 5.3, 2.7, 2.3, 2.6, 1.3, 
          1.8, 1.3, 2.1, 3.2, 2.7, 2.2, 1.4,
          1.0, 1.2, 1.4, 2.1, 2.6, 2.2, 1.1,
          0.7, 0.6, 0.9, 1.2, 1.4, 1.6, 0.8,
          0.7, 0.7, 0.8, 0.9, 1.0, 1.0, 1.0)
cm_prem <- matrix(prem, nrow = 7, ncol = 7)
getr0(q=0.02136927, CM=cm_prem, d=7)

# 2017 VALUES, combined rural and urban
# prem <- c(19.34, 4.54, 1.68, 2.63, 1.76, 1.45, 1.02,
#         3.66, 14.09, 2.86, 1.90, 2.33, 1.92, 1.10, 
#          2.33, 2.19, 6.10, 2.67, 1.90, 1.80, 0.56,
#          2.67, 1.58, 2.81, 3.63, 2.62, 2.10, 0.75, 
#          1.08, 1.31, 1.76, 2.41, 2.69, 2.46, 0.70,
#          0.56, 0.46, 1.04, 1.21, 1.38, 1.78, 0.63,
#          0.39, 0.22, 0.28, 0.32, 0.31, 0.44, 0.33)


### Model 
seirmod <- function(t, t0, parms) {
  with(as.list(c(t0, parms)), {
    
    # 1. track total sizes of each age group
    num.1 <- s.num.1 + s.v.num.1 + i.num.1 + r.num.1
    num.2 <- s.num.2 + s.v.num.2 + i.num.2 + r.num.2
    num.3 <- s.num.3 + s.v.num.3 + i.num.3 + r.num.3
    num.4 <- s.num.4 + s.v.num.4 + i.num.4 + r.num.4
    num.5 <- s.num.5 + s.v.num.5 + i.num.5 + r.num.5
    num.6 <- s.num.6 + s.v.num.6 + i.num.6 + r.num.6
    num.7 <- s.num.7 + s.v.num.7 + i.num.7 + r.num.7
    
    # 2. define age-specific forces of infection
    # force of infection for 0-9 contacts (infecting the 0-9 year olds, infecting the contact)
    lambda.1 <- 
      q*c11*(i.num.1/num.1) + 
      q*c12*(i.num.2/num.2) + 
      q*c13*(i.num.3/num.3) + 
      q*c14*(i.num.4/num.4) +
      q*c15*(i.num.5/num.5) + 
      q*c16*(i.num.6/num.6) +
      q*c17*(i.num.7/num.7)
    
    lambda.v.1 <- 
      (1-psi)*q*c11*(i.num.1/num.1) + 
      (1-psi)*q*c12*(i.num.2/num.2) + 
      (1-psi)*q*c13*(i.num.3/num.3) + 
      (1-psi)*q*c14*(i.num.4/num.4) +
      (1-psi)*q*c15*(i.num.5/num.5) + 
      (1-psi)*q*c16*(i.num.6/num.6) +
      (1-psi)*q*c17*(i.num.7/num.7)
    
    # force of infection for 10-19 contacts (infecting the 10-19 year olds, infecting the contact)
    lambda.2 <- 
      q*c21*(i.num.1/num.1) + 
      q*c22*(i.num.2/num.2) + 
      q*c23*(i.num.3/num.3) + 
      q*c24*(i.num.4/num.4) +
      q*c25*(i.num.5/num.5) + 
      q*c26*(i.num.6/num.6) +
      q*c27*(i.num.7/num.7)
      
    lambda.v.2 <- 
      (1-psi)*q*c21*(i.num.1/num.1) + 
      (1-psi)*q*c22*(i.num.2/num.2) + 
      (1-psi)*q*c23*(i.num.3/num.3) + 
      (1-psi)*q*c24*(i.num.4/num.4) +
      (1-psi)*q*c25*(i.num.5/num.5) + 
      (1-psi)*q*c26*(i.num.6/num.6) +
      (1-psi)*q*c27*(i.num.7/num.7) 
    
    # force of infection for 20-29 contacts
    lambda.3 <- 
      q*c31*(i.num.1/num.1) + 
      q*c32*(i.num.2/num.2) + 
      q*c33*(i.num.3/num.3) + 
      q*c34*(i.num.4/num.4) +
      q*c35*(i.num.5/num.5) + 
      q*c36*(i.num.6/num.6) +
      q*c37*(i.num.7/num.7) 
    
    lambda.v.3 <- 
      (1-psi)*q*c31*(i.num.1/num.1) + 
      (1-psi)*q*c32*(i.num.2/num.2) + 
      (1-psi)*q*c33*(i.num.3/num.3) + 
      (1-psi)*q*c34*(i.num.4/num.4) +
      (1-psi)*q*c35*(i.num.5/num.5) + 
      (1-psi)*q*c36*(i.num.6/num.6) +
      (1-psi)*q*c37*(i.num.7/num.7)
    
    # force of infection for 30-39 contacts
    lambda.4 <- 
      q*c41*(i.num.1/num.1) + 
      q*c42*(i.num.2/num.2) + 
      q*c43*(i.num.3/num.3) + 
      q*c44*(i.num.4/num.4) +
      q*c45*(i.num.5/num.5) + 
      q*c46*(i.num.6/num.6) +
      q*c47*(i.num.7/num.7)
    
    lambda.v.4 <- (1-psi)*q*c41*(i.num.1/num.1) + 
      (1-psi)*q*c42*(i.num.2/num.2) + 
      (1-psi)*q*c43*(i.num.3/num.3) + 
      (1-psi)*q*c44*(i.num.4/num.4) +
      (1-psi)*q*c45*(i.num.5/num.5) + 
      (1-psi)*q*c46*(i.num.6/num.6) +
      (1-psi)*q*c47*(i.num.7/num.7)
    
    # force of infection for 40-49 contacts
    lambda.5 <- 
      q*c51*(i.num.1/num.1) + 
      q*c52*(i.num.2/num.2) + 
      q*c53*(i.num.3/num.3) + 
      q*c54*(i.num.4/num.4) +
      q*c55*(i.num.5/num.5) + 
      q*c56*(i.num.6/num.6) +
      q*c57*(i.num.7/num.7)
    
    lambda.v.5 <- 
      (1-psi)*q*c51*(i.num.1/num.1) + 
      (1-psi)*q*c52*(i.num.2/num.2) + 
      (1-psi)*q*c53*(i.num.3/num.3) + 
      (1-psi)*q*c54*(i.num.4/num.4) +
      (1-psi)*q*c55*(i.num.5/num.5) + 
      (1-psi)*q*c56*(i.num.6/num.6) +
      (1-psi)*q*c57*(i.num.7/num.7)
    
    # force of infection for 50-59y contacts
    lambda.6 <- 
      q*c61*(i.num.1/num.1) + 
      q*c62*(i.num.2/num.2) + 
      q*c63*(i.num.3/num.3) + 
      q*c64*(i.num.4/num.4) +
      q*c65*(i.num.5/num.5) + 
      q*c66*(i.num.6/num.6) +
      q*c67*(i.num.7/num.7)
    
    lambda.v.6 <- 
      (1-psi)*q*c61*(i.num.1/num.1) + 
      (1-psi)*q*c62*(i.num.2/num.2) + 
      (1-psi)*q*c63*(i.num.3/num.3) + 
      (1-psi)*q*c64*(i.num.4/num.4) +
      (1-psi)*q*c65*(i.num.5/num.5) + 
      (1-psi)*q*c66*(i.num.6/num.6) +
      (1-psi)*q*c67*(i.num.7/num.7)
    
    # force of infection for 60+y contacts
    lambda.7 <- 
      q*c71*(i.num.1/num.1) + 
      q*c72*(i.num.2/num.2) + 
      q*c73*(i.num.3/num.3) + 
      q*c74*(i.num.4/num.4) +
      q*c75*(i.num.5/num.5) + 
      q*c76*(i.num.6/num.6) +
      q*c77*(i.num.7/num.7)
    
    lambda.v.7 <- 
      (1-psi)*q*c71*(i.num.1/num.1) + 
      (1-psi)*q*c72*(i.num.2/num.2) + 
      (1-psi)*q*c73*(i.num.3/num.3) + 
      (1-psi)*q*c74*(i.num.4/num.4) +
      (1-psi)*q*c75*(i.num.5/num.5) + 
      (1-psi)*q*c76*(i.num.6/num.6) +
      (1-psi)*q*c77*(i.num.7/num.7)
    
    # 3. differential equations 
    dS.1 <- -lambda.1*s.num.1 
    dS.v.1 <- -lambda.v.1*s.v.num.1
    dI.1 <- lambda.1*s.num.1 + lambda.v.1*s.v.num.1 - gamma*i.num.1
    dR.1 <- gamma*i.num.1
    
    dS.2 <- -lambda.2*s.num.2
    dS.v.2 <- -lambda.v.2*s.v.num.2 
    dI.2 <- lambda.2*s.num.2 + lambda.v.2*s.v.num.2 - gamma*i.num.2
    dR.2 <- gamma*i.num.2
    
    dS.3 <- -lambda.3*s.num.3
    dS.v.3 <- -lambda.v.3*s.v.num.3
    dI.3 <- lambda.3*s.num.3 + lambda.v.3*s.v.num.3 - gamma*i.num.3
    dR.3 <- gamma*i.num.3
    
    dS.4 <- -lambda.4*s.num.4
    dS.v.4 <- -lambda.v.4*s.v.num.4
    dI.4 <- lambda.4*s.num.4 + lambda.v.4*s.v.num.4 - gamma*i.num.4
    dR.4 <- gamma*i.num.4
    
    dS.5 <- -lambda.5*s.num.5
    dS.v.5 <- -lambda.v.5*s.v.num.5 
    dI.5 <- lambda.5*s.num.5 + lambda.v.5*s.v.num.5 - gamma*i.num.5
    dR.5 <- gamma*i.num.5
    
    dS.6 <- -lambda.6*s.num.6
    dS.v.6 <- -lambda.v.6*s.v.num.6
    dI.6 <- lambda.6*s.num.6 + lambda.v.6*s.v.num.6 - gamma*i.num.6
    dR.6 <- gamma*i.num.6
    
    dS.7 <- -lambda.7*s.num.7
    dS.v.7 <- -lambda.v.7*s.v.num.7
    dI.7 <- lambda.7*s.num.7 + lambda.v.7*s.v.num.7 - gamma*i.num.7
    dR.7 <- gamma*i.num.7
    
    
    # 4. List outputs
    list(c(dS.1, dS.v.1, dI.1, dR.1,
           dS.2, dS.v.2, dI.2, dR.2,
           dS.3, dS.v.3, dI.3, dR.3,
           dS.4, dS.v.4, dI.4, dR.4,
           dS.5, dS.v.5, dI.5, dR.5,
           dS.6, dS.v.6, dI.6, dR.6,
           dS.7, dS.v.7, dI.7, dR.7,
           si.flow.1 = lambda.1*s.num.1 + lambda.v.1*s.v.num.1,
           si.flow.2 = lambda.2*s.num.2 + lambda.v.2*s.v.num.2,
           si.flow.3 = lambda.3*s.num.3 + lambda.v.3*s.v.num.3,
           si.flow.4 = lambda.4*s.num.4 + lambda.v.4*s.v.num.4,
           si.flow.5 = lambda.5*s.num.5 + lambda.v.6*s.v.num.5,
           si.flow.6 = lambda.6*s.num.6 + lambda.v.6*s.v.num.6,
           si.flow.7 = lambda.7*s.num.7 + lambda.v.7*s.v.num.7,
           svi.flow.1 = lambda.v.1*s.v.num.1, 
           svi.flow.2 = lambda.v.2*s.v.num.2,
           svi.flow.3 = lambda.v.3*s.v.num.3, 
           svi.flow.4 = lambda.v.4*s.v.num.4, 
           svi.flow.5 = lambda.v.5*s.v.num.5, 
           svi.flow.6 = lambda.v.6*s.v.num.6,
           svi.flow.7 = lambda.v.7*s.v.num.7
    ))
  })
}

### Parameters
# rural contact patterns
param.rural <- param.dcm(gamma = 1/7, psi = 0.50, q = 0.03323546,
                         c11=2.39, c12=2.21, c13=1.22, c14=1.32, c15=1.15, c16=1.75, c17=1.20,
                         c21=1.63, c22=7.41, c23=2.86, c24=2.28, c25=2.32, c26=2.82, c27=1.79,
                         c31=0.61, c32=1.95, c33=1.72, c34=1.40, c35=1.28, c36=1.50, c37=1.01,
                         c41=0.46, c42=1.08, c43=0.97, c44=2.06, c45=1.73, c46=1.97, c47=1.40,
                         c51=0.25, c52=0.70, c53=0.56, c54=1.10, c55=1.38, c56=1.89, c57=1.19,
                         c61=0.25, c62=0.55, c63=0.43, c64=0.82, c65=1.23, c66=1.60, c67=0.92,
                         c71=0.18, c72=0.36, c73=0.30, c74=0.60, c75=0.81, c76=0.96, c77=0.98)

# urban contact patterns
param.urban <- param.dcm(gamma = 1/7, psi = 0.50, q = 0.04852599,
                         c11=1.35, c12=1.35, c13=1.39, c14=1.41, c15=0.65, c16=1.03, c17=0.79,
                         c21=0.99, c22=5.06, c23=1.78, c24=1.93, c25=1.37, c26=2.15, c27=1.15,
                         c31=0.70, c32=1.22, c33=0.88, c34=1.11, c35=1.06, c36=1.05, c37=0.81,
                         c41=0.49, c42=0.91, c43=0.77, c44=1.87, c45=1.05, c46=1.11, c47=0.68,
                         c51=0.14, c52=0.41, c53=0.47, c54=0.67, c55=0.78, c56=1.10, c57=0.50,
                         c61=0.15, c62=0.42, c63=0.30, c64=0.46, c65=0.72, c66=0.90, c67=0.51,
                         c71=0.12, c72=0.23, c73=0.24, c74=0.29, c75=0.34, c76=0.53, c77=0.80)

# prem et al contact patterns
param.prem <- param.dcm(gamma = 1/7, psi = 0.50, q = 0.02136927,
                        c11=7.5, c12=3.4, c13=2.2, c14=3.5, c15=2.4, c16=2.1, c17=1.8,
                        c21=2.5, c22=11.2, c23=3.1, c24=2.3, c25=2.8, c26=2.2, c27=1.5,
                        c31=1.7, c32=1.7, c33=5.3, c34=2.7, c35=2.3, c36=2.6, c37=1.3,
                        c41=1.8, c42=1.3, c43=2.1, c44=3.2, c45=2.7, c46=2.2, c47=1.4,
                        c51=1.0, c52=1.2, c53=1.4, c54=2.1, c55=2.6, c56=2.2, c57=1.1,
                        c61=0.7, c62=0.6, c63=0.9, c64=1.2, c65=1.4, c66=1.6, c67=0.8,
                        c71=0.7, c72=0.7, c73=0.8, c74=0.9, c75=1.0, c76=1.0, c77=1.0)


### Initial conditions
# rural with vaccine
init.vax.rur <- init.dcm(s.num.1 = 0.50*6782044, s.v.num.1 = 0.50*6782044, i.num.1 = 1, r.num.1 = 0,
                         s.num.2 = 0.50*5090656, s.v.num.2 = 0.50*5090656, i.num.2 = 1, r.num.2 = 0,
                         s.num.3 = 0.50*3120038, s.v.num.3 = 0.50*3120038, i.num.3 = 1, r.num.3 = 0,
                         s.num.4 = 0.50*1917767, s.v.num.4 = 0.50*1917767, i.num.4 = 1, r.num.4 = 0,
                         # to get s.num in 40-49 and 50-59, we get pop.size_40-59/2; we assume that
                         # each age group has half number of people in 40-59 agr group
                         s.num.5 = 0.50*2349159/2, s.v.num.5 = 0.50*2349159/2, i.num.5 = 1, r.num.5 = 0,
                         s.num.6 = 0.50*2349159/2, s.v.num.6 = 0.50*2349159/2, i.num.6 = 1, r.num.6 = 0,
                         s.num.7 = 0.50*1013562, s.v.num.7 = 0.50*1013562, i.num.7 = 1, r.num.7 = 0,
                         si.flow.1 = 0, si.flow.2 = 0, si.flow.3 = 0, si.flow.4 = 0,
                         si.flow.5 = 0, si.flow.6 = 0, si.flow.7 = 0,
                         svi.flow.1 = 0, svi.flow.2 = 0, svi.flow.3 = 0, svi.flow.4 = 0,
                         svi.flow.5 = 0, svi.flow.6 = 0, svi.flow.7 = 0)
# rural with no vaccine
init.novax.rur <- init.dcm(s.num.1 = 6782044, s.v.num.1 = 0, i.num.1 = 1, r.num.1 = 0,
                           s.num.2 = 5090656, s.v.num.2 = 0, i.num.2 = 1, r.num.2 = 0,
                           s.num.3 = 3120038, s.v.num.3 = 0, i.num.3 = 1, r.num.3 = 0,
                           s.num.4 = 1917767, s.v.num.4 = 0, i.num.4 = 1, r.num.4 = 0,
                           s.num.5 = 2349159/2, s.v.num.5 = 0, i.num.5 = 1, r.num.5 = 0,
                           s.num.6 = 2349159/2, s.v.num.6 = 0, i.num.6 = 1, r.num.6 = 0,
                           s.num.7 = 1013562, s.v.num.7 = 0,  i.num.7 = 1, r.num.7 = 0,
                           si.flow.1 = 0, si.flow.2 = 0, si.flow.3 = 0, si.flow.4 = 0,
                           si.flow.5 = 0, si.flow.6 = 0, si.flow.7 = 0,
                           svi.flow.1 = 0, svi.flow.2 = 0, svi.flow.3 = 0, svi.flow.4 = 0,
                           svi.flow.5 = 0, svi.flow.6 = 0, svi.flow.7 = 0)

# urban with vaccine
init.vax.urb <- init.dcm(s.num.1 = 0.50*2884776, s.v.num.1 = 0.50*2884776, i.num.1 = 1, r.num.1 = 0,
                         s.num.2 = 0.50*2589432, s.v.num.2 = 0.50*2589432, i.num.2 = 1, r.num.2 = 0,
                         s.num.3 = 0.50*2024202, s.v.num.3 = 0.50*2024202, i.num.3 = 1, r.num.3 = 0,
                         s.num.4 = 0.50*1317494, s.v.num.4 = 0.50*1317494, i.num.4 = 1, r.num.4 = 0,
                         s.num.5 = 0.50*1275364/2, s.v.num.5 = 0.50*1275364/2, i.num.5 = 1, r.num.5 = 0,
                         s.num.6 = 0.50*1275364/2, s.v.num.6 = 0.50*1275364/2, i.num.6 = 1, r.num.6 = 0,
                         s.num.7 = 0.50*467750, s.v.num.7 = 0.50*467750, i.num.7 = 1, r.num.7 = 0,
                         si.flow.1 = 0, si.flow.2 = 0, si.flow.3 = 0, si.flow.4 = 0,
                         si.flow.5 = 0, si.flow.6 = 0, si.flow.7 = 0,
                         svi.flow.1 = 0, svi.flow.2 = 0, svi.flow.3 = 0, svi.flow.4 = 0,
                         svi.flow.5 = 0, svi.flow.6 = 0, svi.flow.7 = 0)

# urban with no vaccine
init.novax.urb <- init.dcm(s.num.1 = 2884776, s.v.num.1 = 0, i.num.1 = 1, r.num.1 = 0,
                           s.num.2 = 2589432, s.v.num.2 = 0, i.num.2 = 1, r.num.2 = 0,
                           s.num.3 = 2024202, s.v.num.3 = 0, i.num.3 = 1, r.num.3 = 0,
                           s.num.4 = 1317494, s.v.num.4 = 0, i.num.4 = 1, r.num.4 = 0,
                           s.num.5 = 1275364/2, s.v.num.5 = 0, i.num.5 = 1, r.num.5 = 0,
                           s.num.6 = 1275364/2, s.v.num.6 = 0, i.num.6 = 1, r.num.6 = 0,
                           s.num.7 = 467750, s.v.num.7 = 0, i.num.7 = 1, r.num.7 = 0,
                           si.flow.1 = 0, si.flow.2 = 0, si.flow.3 = 0, si.flow.4 = 0,
                           si.flow.5 = 0, si.flow.6 = 0, si.flow.7 = 0,  
                           svi.flow.1 = 0, svi.flow.2 = 0, svi.flow.3 = 0, svi.flow.4 = 0,
                           svi.flow.5 = 0, svi.flow.6 = 0, svi.flow.7 = 0)

# prem et al with vaccine
init.vax.prem <- init.dcm(s.num.1 = 0.50*9666820, s.v.num.1 = 0.50*9666820, i.num.1 = 1, r.num.1 = 0,
                          s.num.2 = 0.50*7680088, s.v.num.2 = 0.50*7680088, i.num.2 = 1, r.num.2 = 0,
                          s.num.3 = 0.50*5144240, s.v.num.3 = 0.50*5144240, i.num.3 = 1, r.num.3 = 0,
                          s.num.4 = 0.50*3235261, s.v.num.4 = 0.50*3235261, i.num.4 = 1, r.num.4 = 0,
                          s.num.5 = 0.50*3624523/2, s.v.num.5 = 0.50*3624523/2, i.num.5 = 1, r.num.5 = 0,
                          s.num.6 = 0.50*3624523/2, s.v.num.6 = 0.50*3624523/2, i.num.6 = 1, r.num.6 = 0,
                          s.num.7 = 0.50*1481312, s.v.num.7 = 0.50*1481312, i.num.7 = 1, r.num.7 = 0,
                          si.flow.1 = 0, si.flow.2 = 0, si.flow.3 = 0, si.flow.4 = 0, si.flow.5 = 0,
                          si.flow.6 = 0, si.flow.7 = 0,
                          svi.flow.1 = 0, svi.flow.2 = 0, svi.flow.3 = 0, svi.flow.4 = 0,
                          svi.flow.5 = 0, svi.flow.6 = 0, svi.flow.7 = 0)

# prem et al with no vaccine
init.novax.prem <- init.dcm(s.num.1 = 9666820, s.v.num.1 = 0, i.num.1 = 1, r.num.1 = 0,
                            s.num.2 = 7680088, s.v.num.2 = 0, i.num.2 = 1, r.num.2 = 0,
                            s.num.3 = 5144240, s.v.num.3 = 0, i.num.3 = 1, r.num.3 = 0,
                            s.num.4 = 3235261, s.v.num.4 = 0, i.num.4 = 1, r.num.4 = 0,
                            s.num.5 = 3624523/2, s.v.num.5 = 0, i.num.5 = 1, r.num.5 = 0,
                            s.num.6 = 3624523/2, s.v.num.6 = 0, i.num.6 = 1, r.num.6 = 0,
                            s.num.7 = 1481312, s.v.num.7 = 0, i.num.7 = 1, r.num.7 = 0,
                            si.flow.1 = 0, si.flow.2 = 0, si.flow.3 = 0, si.flow.4 = 0, 
                            si.flow.5 = 0, si.flow.6 = 0, si.flow.7 = 0,
                            svi.flow.1 = 0, svi.flow.2 = 0, svi.flow.3 = 0, svi.flow.4 = 0,
                            svi.flow.5 = 0, svi.flow.6 = 0, svi.flow.7 = 0)

### Controls 
control <- control.dcm(nstep=360, new.mod = seirmod)

### Compile model GlobalMix rural
sim.vax.rur <- dcm(param.rural, init.vax.rur, control)
df.vax.rur <- as.data.frame(sim.vax.rur)
sim.novax.rur <- dcm(param.rural, init.novax.rur, control)
df.novax.rur <- as.data.frame(sim.novax.rur)

### Compile model GlobalMix urban
sim.vax.urban <- dcm(param.urban, init.vax.urb, control)
df.vax.urban <- as.data.frame(sim.vax.urban)
sim.novax.urban <- dcm(param.urban, init.novax.urb, control)
df.novax.urban <- as.data.frame(sim.novax.urban)

### Compile model Prem et al
sim.vax.prem <- dcm(param.prem, init.vax.prem, control)
df.vax.prem <- as.data.frame(sim.vax.prem)
sim.novax.prem <- dcm(param.prem, init.novax.prem, control)
df.novax.prem <- as.data.frame(sim.novax.prem)



### results
# function for no vaccine attack rate
AR <- function(novax.si.flow, pop){
  (sum(novax.si.flow) / pop) * 100
}

# function for vaccine attack rate
ARv <- function(vax.si.flow, pop){
  (sum(vax.si.flow) / pop) * 100
}

# overall vaccine effect
overall.eff <- function(vax.si.flow, novax.si.flow){
  (1 - (sum(vax.si.flow) / sum(novax.si.flow))) * 100
}

### age group 0-9
# rural
AR.rur.0_9 <- print(AR(df.novax.rur$si.flow.1, init.novax.rur$s.num.1)) 
ARv.rur.0_9 <- print(ARv(df.vax.rur$si.flow.1, init.novax.rur$s.num.1)) 
ov.eff.rur.0_9 <- print(overall.eff(df.vax.rur$si.flow.1, df.novax.rur$si.flow.1)) 

# urban
AR.urb.0_9 <- print(AR(df.novax.urban$si.flow.1, init.novax.urb$s.num.1)) 
ARv.urb.0_9 <- print(ARv(df.vax.urban$si.flow.1, init.novax.urb$s.num.1)) 
ov.eff.urb.0_9 <- print(overall.eff(df.vax.urban$si.flow.1, df.novax.urban$si.flow.1)) 

# prem
AR.prem.0_9 <- print(AR(df.novax.prem$si.flow.1, init.novax.prem$s.num.1)) 
ARv.prem.0_9 <- print(ARv(df.vax.prem$si.flow.1, init.novax.prem$s.num.1)) 
ov.eff.prem.0_9 <- print(overall.eff(df.vax.prem$si.flow.1, df.novax.prem$si.flow.1)) 

### age group 10-19
# rural
AR.rur.10_19 <- print(AR(df.novax.rur$si.flow.2, init.novax.rur$s.num.2)) 
ARv.rur.10_19 <- print(ARv(df.vax.rur$si.flow.2, init.novax.rur$s.num.2)) 
ov.eff.rur.10_19 <- print(overall.eff(df.vax.rur$si.flow.2, df.novax.rur$si.flow.2)) 

# urban
AR.urb.10_19 <- print(AR(df.novax.urban$si.flow.2, init.novax.urb$s.num.2)) 
ARv.urb.10_19 <- print(ARv(df.vax.urban$si.flow.2, init.novax.urb$s.num.2)) 
ov.eff.urb.10_19 <- print(overall.eff(df.vax.urban$si.flow.2, df.novax.urban$si.flow.2)) 

# prem
AR.prem.10_19 <- print(AR(df.novax.prem$si.flow.2, init.novax.prem$s.num.2)) 
ARv.prem.10_19 <- print(ARv(df.vax.prem$si.flow.2, init.novax.prem$s.num.2)) 
ov.eff.prem.10_19 <- print(overall.eff(df.vax.prem$si.flow.2, df.novax.prem$si.flow.2)) 


### age group 20-29
# rural
AR.rur.20_29 <- print(AR(df.novax.rur$si.flow.3, init.novax.rur$s.num.3)) 
ARv.rur.20_29 <- print(ARv(df.vax.rur$si.flow.3, init.novax.rur$s.num.3)) 
ov.eff.rur.20_29 <- print(overall.eff(df.vax.rur$si.flow.3, df.novax.rur$si.flow.3)) 

# urban
AR.urb.20_29 <- print(AR(df.novax.urban$si.flow.3, init.novax.urb$s.num.3)) 
ARv.urb.20_29 <- print(ARv(df.vax.urban$si.flow.3, init.novax.urb$s.num.3)) 
ov.eff.urb.20_29 <- print(overall.eff(df.vax.urban$si.flow.3, df.novax.urban$si.flow.3)) 

# prem
AR.prem.20_29 <- print(AR(df.novax.prem$si.flow.3, init.novax.prem$s.num.3)) 
ARv.prem.20_29 <- print(ARv(df.vax.prem$si.flow.3, init.novax.prem$s.num.3)) 
ov.eff.prem.20_29 <- print(overall.eff(df.vax.prem$si.flow.3, df.novax.prem$si.flow.3)) 


### age group 30-39
# rural
AR.rur.30_39 <- print(AR(df.novax.rur$si.flow.4, init.novax.rur$s.num.4)) 
ARv.rur.30_39 <- print(ARv(df.vax.rur$si.flow.4, init.novax.rur$s.num.4)) 
ov.eff.rur.30_39 <- print(overall.eff(df.vax.rur$si.flow.4, df.novax.rur$si.flow.4)) 

# urban
AR.urb.30_39 <- print(AR(df.novax.urban$si.flow.4, init.novax.urb$s.num.4)) 
ARv.urb.30_39 <- print(ARv(df.vax.urban$si.flow.4, init.novax.urb$s.num.4)) 
ov.eff.urb.30_39 <- print(overall.eff(df.vax.urban$si.flow.4, df.novax.urban$si.flow.4)) 

# prem
AR.prem.30_39 <- print(AR(df.novax.prem$si.flow.4, init.novax.prem$s.num.4)) 
ARv.prem.30_39 <- print(ARv(df.vax.prem$si.flow.4, init.novax.prem$s.num.4)) 
ov.eff.prem.30_39 <- print(overall.eff(df.vax.prem$si.flow.4, df.novax.prem$si.flow.4)) 


### age group 40-49
# rural
AR.rur.40_49 <- print(AR(df.novax.rur$si.flow.5, init.novax.rur$s.num.5)) 
ARv.rur.40_49 <- print(ARv(df.vax.rur$si.flow.5, init.novax.rur$s.num.5))
ov.eff.rur.40_49 <- print(overall.eff(df.vax.rur$si.flow.5, df.novax.rur$si.flow.5)) 

# urban
AR.urb.40_49 <- print(AR(df.novax.urban$si.flow.5, init.novax.urb$s.num.5)) 
ARv.urb.40_49 <- print(ARv(df.vax.urban$si.flow.5, init.novax.urb$s.num.5)) 
ov.eff.urb.40_49 <- print(overall.eff(df.vax.urban$si.flow.5, df.novax.urban$si.flow.5)) 

# prem
AR.prem.40_49 <- print(AR(df.novax.prem$si.flow.5, init.novax.prem$s.num.5)) 
ARv.prem.40_49 <- print(ARv(df.vax.prem$si.flow.5, init.novax.prem$s.num.5)) 
ov.eff.prem.40_49 <- print(overall.eff(df.vax.prem$si.flow.5, df.novax.prem$si.flow.5)) 


### age group 50-59
# rural
AR.rur.50_59 <- print(AR(df.novax.rur$si.flow.6, init.novax.rur$s.num.6)) 
ARv.rur.50_59 <- print(ARv(df.vax.rur$si.flow.6, init.novax.rur$s.num.6))
ov.eff.rur.50_59 <- print(overall.eff(df.vax.rur$si.flow.6, df.novax.rur$si.flow.6)) 

# urban
AR.urb.50_59 <- print(AR(df.novax.urban$si.flow.6, init.novax.urb$s.num.6)) 
ARv.urb.50_59 <- print(ARv(df.vax.urban$si.flow.6, init.novax.urb$s.num.6)) 
ov.eff.urb.50_59 <- print(overall.eff(df.vax.urban$si.flow.6, df.novax.urban$si.flow.6)) 

# prem
AR.prem.50_59 <- print(AR(df.novax.prem$si.flow.6, init.novax.prem$s.num.6)) 
ARv.prem.50_59 <- print(ARv(df.vax.prem$si.flow.6, init.novax.prem$s.num.6)) 
ov.eff.prem.50_59 <- print(overall.eff(df.vax.prem$si.flow.6, df.novax.prem$si.flow.6))


### age group 60+y
# rural
AR.rur.60 <- print(AR(df.novax.rur$si.flow.7, init.novax.rur$s.num.7)) 
ARv.rur.60 <- print(ARv(df.vax.rur$si.flow.7, init.novax.rur$s.num.7)) 
ov.eff.rur.60 <- print(overall.eff(df.vax.rur$si.flow.7, df.novax.rur$si.flow.7)) 

# urban
AR.urb.60 <- print(AR(df.novax.urban$si.flow.7, init.novax.urb$s.num.7)) 
ARv.urb.60 <- print(ARv(df.vax.urban$si.flow.7, init.novax.urb$s.num.7)) 
ov.eff.urb.60 <- print(overall.eff(df.vax.urban$si.flow.7, df.novax.urban$si.flow.7)) 

# prem
AR.prem.60 <- print(AR(df.novax.prem$si.flow.7, init.novax.prem$s.num.7)) 
ARv.prem.60 <- print(ARv(df.vax.prem$si.flow.7, init.novax.prem$s.num.7)) 
ov.eff.prem.60 <- print(overall.eff(df.vax.prem$si.flow.7, df.novax.prem$si.flow.7)) 

# create a dataframe with the attack rates RURAL 
AR.rural <- rbind(AR.rur.0_9, AR.rur.10_19, AR.rur.20_29, AR.rur.30_39, 
                  AR.rur.40_49, AR.rur.50_59, AR.rur.60)
ARV.rural <- rbind(ARv.rur.0_9, ARv.rur.10_19, ARv.rur.20_29, ARv.rur.30_39,
                   ARv.rur.40_49, ARv.rur.50_59, ARv.rur.60)

allAR.rural <- cbind(AR.rural, ARV.rural)
allAR.rural.melt <- reshape2::melt(allAR.rural, id.vars="Xax")
allAR.rural.melt <- allAR.rural.melt %>%
  mutate(vax = case_when(Var2 == 1 ~ "No Vaccine",
                         Var2 == 2 ~ "Vaccine")) %>%
  mutate(age_group = case_when(Var1 == "AR.rur.0_9" ~ "0-9y",
                               Var1 == "AR.rur.10_19" ~ "10-19y",
                               Var1 == "AR.rur.20_29" ~ "20-29y",
                               Var1 == "AR.rur.30_39" ~ "30-39y",
                               Var1 == "AR.rur.40_49" ~ "40-49y",
                               Var1 == "AR.rur.50_59" ~ "50-59y",
                               Var1 == "AR.rur.60" ~ "60+y")) %>%
  select(age_group, vax, value)

# rural.ve <- c("26%", "13%", "30%", "32%", "49%", "41%")
# rural.ve <- c("31%", "56%", "26%", "22%", "15%", "13%", "9%")

# create a dataframe with the attack rates URBAN 
AR.urban <- rbind(AR.urb.0_9, AR.urb.10_19, AR.urb.20_29, AR.urb.30_39, 
                  AR.urb.40_49, AR.urb.50_59, AR.urb.60)
ARV.urban <- rbind(ARv.urb.0_9, ARv.urb.10_19, ARv.urb.20_29, ARv.urb.30_39, 
                   ARv.urb.40_49, ARv.urb.50_59, ARv.urb.60)

allAR.urban <- cbind(AR.urban, ARV.urban)
allAR.urban.melt <- reshape2::melt(allAR.urban, id.vars="Xax")
allAR.urban.melt <- allAR.urban.melt %>%
  mutate(vax = case_when(Var2 == 1 ~ "No Vaccine",
                         Var2 == 2 ~ "Vaccine")) %>%
  mutate(age_group = case_when(Var1 == "AR.urb.0_9" ~ "0-9y",
                               Var1 == "AR.urb.10_19" ~ "10-19y",
                               Var1 == "AR.urb.20_29" ~ "20-29y",
                               Var1 == "AR.urb.30_39" ~ "30-39y",
                               Var1 == "AR.urb.40_49" ~ "40-49y",
                               Var1 == "AR.urb.50_59" ~ "50-59y",
                               Var1 == "AR.urb.60" ~ "60+y")) %>%
  select(age_group, vax, value)


# create a dataframe with the attack rates PREM
AR.prem <- rbind(AR.prem.0_9, AR.prem.10_19, AR.prem.20_29, AR.prem.30_39, 
                 AR.prem.40_49, AR.prem.50_59, AR.prem.60)
ARV.prem <- rbind(ARv.prem.0_9, ARv.prem.10_19, ARv.prem.20_29, ARv.prem.30_39, 
                  ARv.prem.40_49, ARv.prem.50_59, ARv.prem.60)

allAR.prem <- cbind(AR.prem, ARV.prem)
allAR.prem.melt <- reshape2::melt(allAR.prem, id.vars="Xax")
allAR.prem.melt <- allAR.prem.melt %>%
  mutate(vax = case_when(Var2 == 1 ~ "No Vaccine",
                         Var2 == 2 ~ "Vaccine")) %>%
  mutate(age_group = case_when(Var1 == "AR.prem.0_9" ~ "0-9y",
                               Var1 == "AR.prem.10_19" ~ "10-19y",
                               Var1 == "AR.prem.20_29" ~ "20-29y",
                               Var1 == "AR.prem.30_39" ~ "30-39y",
                               Var1 == "AR.prem.40_49" ~ "40-49y",
                               Var1 == "AR.prem.50_59" ~ "50-59y",
                               Var1 == "AR.prem.60" ~ "60+y")) %>%
  select(age_group, vax, value)

# function to create dot plot for attack rates
fxn_create_dot_plot <- function(data) {
  segment_helper <- data %>%
    mutate(vax = case_when(vax == "Vaccine" ~ "Yes",
                           vax == "No Vaccine" ~ "No"),
           vax = factor(vax,
                        levels = c("Yes", "No"))) %>%
    pivot_wider(names_from = vax, values_from = value, names_prefix = 'v_') %>%
    mutate(change = v_Yes - v_No )
  
  ggplot() + 
    geom_segment(data = segment_helper,
                 aes(x = v_No, xend = v_Yes, y = age_group, yend = age_group),
                 col = 'grey60',
                 linewidth = 1.25) +
    geom_point(
      data = data,
      aes(x = value, y = age_group, col = vax), size = 4) +
    axis_text_theme2 +
    labs(
      x = 'Attack rate (%)',
      y = 'Age group') +
   scale_color_manual(values = cols_model) +
    scale_x_continuous(limits = c(0, 100)) +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.minor.x = element_blank(),
      axis.text.x = element_text(angle = 0),
      legend.position = "right",
      legend.direction = "vertical")
    # scale_x_continuous(expand = expansion(mult = 0.01))
}

# plot
AR.rural.plot <- fxn_create_dot_plot(allAR.rural.melt) +
  labs(x = '')
AR.rural.plot

AR.urban.plot <- fxn_create_dot_plot(allAR.urban.melt) +
  labs(x = '')
AR.urban.plot

AR.prem.plot <- fxn_create_dot_plot(allAR.prem.melt)
AR.prem.plot  

# combine all plots and matrices into one figure
rural_model <- ruralmatrix | AR.rural.plot
urban_model <- urbanmatrix | AR.urban.plot
prem_model <- premmatrix | AR.prem.plot


fig_model <- wrap_plots(rural_model,
                        urban_model,
                        prem_model) + 
  plot_annotation(tag_levels = 'A') + 
  theme(plot.tag = element_text(size = 12)) +
  plot_layout(nrow=3, heights = c(800, 800, 800))
# fig_model
# 
# ggsave(fig_model, filename = "../../output/figs/fig_modelplot.pdf",
#        height=8, width=8, dpi=300,
#        bg="#FFFFFF")




# Create a dataframe with rural, urban and prem vaccine ARs
ARV <- cbind(ARV.rural, ARV.urban, ARV.prem) %>%
  reshape2::melt(id.vvars="Xax") %>%
  mutate(site = case_when(Var2 == 1 ~ "Rural",
                          Var2 == 2 ~ "Urban",
                          Var2 == 3 ~ "Prem")) %>%
  mutate(age_group = case_when(Var1 == "AR.rur.0_9" ~ "0-9y",
                               Var1 == "AR.rur.10_19" ~ "10-19y",
                               Var1 == "AR.rur.20_29" ~ "20-29y",
                               Var1 == "AR.rur.30_39" ~ "30-39y",
                               Var1 == "AR.rur.40_49" ~ "40-49y",
                               Var1 == "AR.rur.50_59" ~ "50-59y",
                               Var1 == "AR.rur.60" ~ "60+y"))

data2 <- ARV %>%
  # filter(site != "Urban") %>%
  select(Var1, site, value) %>%
  mutate(site = factor(site,
                       levels = c("Rural", "Urban", "Prem"))) %>%
  pivot_wider(names_from = site, values_from = value) %>%
  mutate(change_prem_rural = Prem - Rural,
         change_prem_urban = Prem - Urban) %>%
  as.data.frame() %>%
  mutate(age_group = case_when(Var1 == AR.rur.0_9 ~ "0-9y",
                               Var1 == AR.rur.10_19 ~ "10-19y",
                               Var1 == AR.rur.20_29 ~ "20-29y",
                               Var1 == AR.rur.30_39 ~ "30-39y",
                               Var1 == AR.rur.40_49 ~ "40-49y",
                               Var1 == AR.rur.50_59 ~ "50-59y",
                               Var1 == AR.rur.60 ~ "60+y"))

rural_prem <- ggplot() + 
  geom_segment(data = data2,
               aes(x = Prem, xend = Rural, y = Var1, yend = Var1),
               col = 'grey60',
               linewidth = 1.25) +
  geom_point(
    data = ARV %>% filter(site != "Urban"),
    aes(x = value, y = Var1, col = site), size = 4) +
  axis_text_theme2 +
  labs(
    x = 'Attack rate (%)',
    y = 'Age group') +
  scale_color_manual(values = cols_model) +
  scale_x_continuous(limits = c(0, 100)) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text.x = element_text(angle = 0),
    legend.position = "right",
    legend.direction = "vertical")
rural_prem
# scale_x_continuous(expand = expansion(mult = 0.01))


urban_prem <- ggplot() + 
  geom_segment(data = data2,
               aes(x = Urban, xend = Prem, y = Var1, yend = Var1),
               col = 'grey60',
               linewidth = 1.25) +
  geom_point(
    data = ARV %>% filter(site != "Rural"),
    aes(x = value, y = Var1, col = site), size = 4) +
  axis_text_theme2 +
  labs(
    x = 'Attack rate (%)',
    y = 'Age group') +
  scale_color_manual(values = cols_model) +
  scale_x_continuous(limits = c(0, 100)) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text.x = element_text(angle = 0),
    legend.position = "right",
    legend.direction = "vertical")
# scale_x_continuous(expand = expansion(mult = 0.01))
urban_prem

sites_vax <- rural_prem / urban_prem

ggsave(sites_vax, filename = "../../output/figs/fig_siteARV_2021data.pdf",
       height=8, width=8, dpi=300,
       bg="#FFFFFF") 


############
# Create a df for overall attack rates
OE.rur <- rbind(ov.eff.rur.0_9, ov.eff.rur.10_19, ov.eff.rur.20_29, ov.eff.rur.30_39, 
                ov.eff.rur.40_49, ov.eff.rur.50_59, ov.eff.rur.60)
OE.urb <- rbind(ov.eff.urb.0_9, ov.eff.urb.10_19, ov.eff.urb.20_29, ov.eff.urb.30_39, 
                ov.eff.urb.40_49, ov.eff.urb.50_59, ov.eff.urb.60)
OE.prem <- rbind(ov.eff.prem.0_9, ov.eff.prem.10_19, ov.eff.prem.20_29, ov.eff.prem.30_39, 
                 ov.eff.prem.40_49, ov.eff.prem.50_59, ov.eff.prem.60)

OE <- cbind(OE.rur, OE.urb, OE.prem) %>%
  reshape2::melt(id.vvars="Xax") %>%
  mutate(site = case_when(Var2 == 1 ~ "Rural",
                          Var2 == 2 ~ "Urban",
                          Var2 == 3 ~ "Prem")) %>%
  mutate(age_group = case_when(Var1 == "ov.eff.rur.0_9" ~ "0-9y",
                               Var1 == "ov.eff.rur.10_19" ~ "10-19y",
                               Var1 == "ov.eff.rur.20_29" ~ "20-29y",
                               Var1 == "ov.eff.rur.30_39" ~ "30-39y",
                               Var1 == "ov.eff.rur.40_49" ~ "40-49y",
                               Var1 == "ov.eff.rur.50_59" ~ "50-59y",
                               Var1 == "ov.eff.rur.60" ~ "60+y"))

data3 <- OE %>%
  # filter(site != "Urban") %>%
  select(Var1, site, value) %>%
  mutate(site = factor(site,
                       levels = c("Rural", "Urban", "Prem"))) %>%
  pivot_wider(names_from = site, values_from = value) %>%
  mutate(change_prem_rural = Prem - Rural,
         change_prem_urban = Prem - Urban) %>%
  as.data.frame() %>%
  mutate(age_group = case_when(Var1 == "ov.eff.rur.0_9" ~ "0-9y",
                               Var1 == "ov.eff.rur.10_19" ~ "10-19y",
                               Var1 == "ov.eff.rur.20_29" ~ "20-29y",
                               Var1 == "ov.eff.rur.30_39" ~ "30-39y",
                               Var1 == "ov.eff.rur.40_49" ~ "40-49y",
                               Var1 == "ov.eff.rur.50_59" ~ "50-59y",
                               Var1 == "ov.eff.rur.60" ~ "60+y")) %>%
  select(-c(Var1))

# combined overall VE for Prem, rural and urban
oe_combined <- ggplot() + 
  geom_segment(data = data3,
               aes(x = Prem, xend = Rural, y = age_group, yend = age_group),
               col = 'grey60',
               linewidth = 1.25) +
  geom_segment(data = data3,
               aes(x = Prem, xend = Urban, y = age_group, yend = age_group),
               col = 'grey60',
               linewidth = 1.25) +
  geom_point(
    data = OE %>% filter(site != "Urban"),
    aes(x = value, y = age_group, col = site), size = 3) +
  geom_point(
    data = OE %>% filter(site != "Rural"),
    aes(x = value, y = age_group, col = site), 
    size = 3,
    shape = 1) +
  axis_text_theme2 +
  
  labs(
    x = 'Overall VE (%)',
    y = 'Age group',
    title = 'Overall VE') +
  scale_color_manual(values = cols_model) +
  scale_x_continuous(limits = c(0, 100)) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text.x = element_text(angle = 0),
    legend.position = c(0.1, 0.8),
    legend.direction = "vertical")
oe_combined

ggsave(oe_combined, filename = "../../output/figs/oe_fig_site_2021data.pdf",
       height=4, width=8, dpi=300,
       bg="#FFFFFF") 

# combine the matrices and VEs
combined_model_figure <- adjusted_matrix / oe_combined

combined_model_figure <- wrap_plots(adjusted_matrix, oe_combined) + 
  plot_annotation(tag_levels = 'A') + 
  theme(plot.tag = element_text(size = 12)) +
  plot_layout(nrow=2, heights = c(600))
combined_model_figure

ggsave(combined_model_figure, filename = "../../output/figs/fig2_matrix_model_2021data.pdf",
       height=6, width=8, dpi=300,
       bg="#FFFFFF") 

# below generates separate oVE per site
oe_rural_prem <- ggplot() + 
  geom_segment(data = data3,
               aes(x = Prem, xend = Rural, y = Var1, yend = Var1),
               col = 'grey60',
               linewidth = 1.25) +
  geom_point(
    data = OE %>% filter(site != "Urban"),
    aes(x = value, y = Var1, col = site), size = 4) +
  # axis_text_theme2 +
  labs(
    x = 'Overall VE (%)',
    y = 'Age group') +
  scale_color_manual(values = cols_model) +
  scale_x_continuous(limits = c(0, 100)) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text.x = element_text(angle = 0),
    legend.position = "right",
    legend.direction = "vertical")
# oe_rural_prem

oe_urban_prem <- ggplot() + 
  geom_segment(data = data3,
               aes(x = Urban, xend = Prem, y = Var1, yend = Var1),
               col = 'grey60',
               linewidth = 1.25) +
  geom_point(
    data = OE %>% filter(site != "Rural"),
    aes(x = value, y = Var1, col = site), size = 4) +
  axis_text_theme2 +
  labs(
    x = 'Overall VE (%)',
    y = 'Age group') +
  scale_color_manual(values = cols_model) +
  scale_x_continuous(limits = c(0, 100)) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text.x = element_text(angle = 0),
    legend.position = "right",
    legend.direction = "vertical")
# oe_urban_prem

# oe_sites_vax <- oe_rural_prem / oe_urban_prem
# 
# ggsave(oe_sites_vax, filename = "../../output/figs/oe_fig_site.pdf",
#        height=8, width=8, dpi=300,
#        bg="#FFFFFF") 
################

cat("End of model script.")


# ALTERNATIVE CODE FOR GRAPHS

# AR.rural.plot
AR.rural.plot2 <- allAR.rural.melt %>%
  filter(vax == "Vaccine") %>%
  ggplot(aes(age_group, value, fill=vax)) +
  geom_bar(stat = "identity", position = 'dodge') +
  geom_hline(yintercept = 26.2, color="black", linetype = "dashed", linewidth=1.3) +
  annotate("text", x=6, y=28,
           label=paste0("VE = 26.2%"),
           hjust=0, size=5) +
  coord_flip() +
  xlab("Age Group") +
  ylab("") +
  labs(fill = NULL)  +
  axis_text_theme2 +
  theme(legend.position = "none",
        axis.text.x = element_text(angle=0))
AR.rural.plot2

# AR.urban.plot
AR.urban.plot2 <- allAR.urban.melt %>%
  filter(vax == "Vaccine") %>%
  ggplot(aes(age_group, value, fill=vax)) +
  coord_flip() +
  geom_bar(stat = "identity", position = 'dodge') +
  geom_hline(yintercept = 26.2, color="black", linetype = "dashed", linewidth=1.3) +
  annotate("text", x=6, y=28,
           label=paste0("VE = 26.2%"),
           hjust=0, size=5) +
  xlab("Age Group") +
  ylab("Attack Rate (%)") +
  labs(fill = NULL) +
  axis_text_theme2 +
  theme(legend.position = "none",
        # axis.text.y = element_blank(),
        # axis.title.y = element_blank(),
        axis.text.x = element_text(angle=0))
AR.urban.plot2

# AR.prem.plot
AR.prem.plot2 <- allAR.prem.melt %>%
  filter(vax == "Vaccine") %>%
  ggplot(aes(age_group, value, fill=vax)) +
  geom_bar(stat = "identity", position = 'dodge') +
  geom_hline(yintercept = 26.9, color="black", linetype = "dashed", linewidth=1.3) +
  annotate("text", x=6, y=28,
           label=paste0("VE = 26.9%"),
           hjust=0, size=5) +
  coord_flip() +
  xlab("Age Group") +
  ylab("Attack Rate (%)") +
  labs(fill = NULL)  +
  axis_text_theme2 +
  theme(legend.position = "right",
        legend.direction = "vertical",
        # axis.text.y = element_blank(),
        # axis.title.y = element_blank(),
        axis.text.x = element_text(angle=0))
AR.prem.plot2

rural_model2 <- ruralmatrix | AR.rural.plot2
urban_model2 <- urbanmatrix | AR.urban.plot2
prem_model2 <- premmatrix | AR.prem.plot2

fig_model2 <- wrap_plots(rural_model2,
                        urban_model2,
                        prem_model2) + 
  plot_annotation(tag_levels = 'A') + 
  theme(plot.tag = element_text(size = 12)) +
  plot_layout(nrow=3, heights = c(800, 800, 800))
fig_model2

ggsave(fig_model2, filename = "../../output/figs/fig_modelplot_v2.pdf",
       height=8, width=8, dpi=300,
       bg="#FFFFFF")

# # ADDED BY MCK TO TEST
# modelplots <- plot_grid(ruralmatrix, urbanmatrix,
#                         AR.rural.plot, AR.urban.plot,
#                         labels = c('GlobalMix Rural', 'GlobalMix Urban',
#                                    'GlobalMix Rural', 'GlobalMix Urban',
#                                    label_size = 12, vjust = 3))

# premmatrix MISSING. WILL NEED SOME REFORMATING E.G. COMBINING LEGENDS AND
# CONFORMING TEXTS TO axis_text-theme2 AVAILABLE IN CUSTOMIZATION CODE.

# model_matrices <- ruralmatrix | urbanmatrix | premmatrix
# model_ar <- AR.rural.plot | AR.urban.plot | AR.prem.plot
# fig_model <- wrap_plots(model_matrices,
#                         model_ar) + 
#   plot_annotation(tag_levels = 'A') + 
#   theme(plot.tag = element_text(size = 12)) +
#   plot_layout(nrow=2, heights = c(600, 600))

