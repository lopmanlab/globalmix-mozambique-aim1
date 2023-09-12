

###############################################################################  
# This file contains scripts to analyze and visualize place use data.
# Author: Sara Kim
# Date: 08/18/2023
############################################################################### 


### Check to see if there are IDs with no contacts reported
sum(!participants$rec_id %in% contacts$rec_id) # 3

missing <- participants %>% 
  filter(!rec_id %in% unlist(contacts$rec_id)) %>% 
  arrange(rec_id)
# 371, 754, 1996

# Remove those without contact diaries
participants <- participants %>%
  filter(!rec_id %in% unlist(missing$rec_id)) 

rm(missing)

### Check those who live alone
live_alone <- household %>% 
  filter(rec_id %in% unlist(participants$rec_id)) %>%
  filter(hh_occupants==1) 
# 35


##### Data cleaning for socialmixr package
contacts <- contacts %>%
  left_join(participants %>% 
              select(rec_id, participant_age), 
            by = c("rec_id"="rec_id")) %>%
  mutate(age_part = participant_age) 

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

contacts <- contacts %>%
  mutate(part_id = rec_id,
         part_age = age_part)


##### Weighting
### age categories to join with population weighting
contacts <- contacts %>%
  mutate(
    part_age = as.numeric(part_age),
    weight_cat = case_when(
      part_age ==1 ~ "0",
      part_age<=4  ~ "1-4y",
      part_age<=9  ~ "5-9y",
      part_age<=14 ~ "10-14y",
      part_age<=19 ~ "15-19y",
      part_age<=29 ~ "20-29y",
      part_age<=39 ~ "30-39y",
      part_age<=59 ~ "40-59y",
      part_age<=100 ~ "60+y",
      TRUE ~ NA_character_
    )
  )

contacts <- contacts %>%
  mutate(
    part_age = as.numeric(part_age),
    weight_cat = case_when(
      participant_age == "<6mo" ~ "0",
      participant_age == "6-11mo" ~ "0",
      TRUE ~ participant_age
    )
  )

### read in population distribution
pop_dist <- read.csv("scripts/manuscript/mozambique_population_distribution.csv") %>%
  pivot_longer(cols=urban:rural, names_to = "urb_rur", values_to = "tot_pop") %>%
  mutate(study_site = case_when(
    urb_rur =="urban" ~"Urban",
    urb_rur == "rural"~"Rural"
  ))

### weight cleaning
pop_weight <- contacts %>%
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
contacts <- contacts %>%
  left_join(pop_weight %>% select(study_site, weight_cat, part_weight),
            by = c("study_site"="study_site", "weight_cat"="weight_cat"))


##### Create survey structures for input into socialmixr package
contacts <- contacts %>% 
  mutate(part_age = as.numeric(part_age),
         contact_age = as.character(contact_age),
         
         cnt_age_est_min = case_when( 
           ## If dont have exact contact age, need to specify a minimum and maximum based on the age group range
           contact_age == "<6mo" ~0,
           contact_age == "6-11mo"~0,
           contact_age == "1-4y" ~1,
           contact_age == "5-9y" ~ 5,
           contact_age == "10-14y"~10,
           contact_age == "15-19y"~15,
           contact_age == "20-29y"~20,
           contact_age == "30-39y"~30,
           contact_age == "40-59y"~40,
           contact_age == "60+y"~60,
           contact_age== "I don't know" ~ NA_real_,
           is.na(contact_age) ~ NA_real_
         ),
         
         cnt_age_est_max= case_when(
           contact_age == "<6mo" ~0,
           contact_age == "6-11mo"~1,
           contact_age == "1-4y" ~ 4,
           contact_age == "5-9y" ~ 9,
           contact_age == "10-14y"~14,
           contact_age == "15-19y"~19,
           contact_age == "20-29y"~29,
           contact_age == "30-39y"~39,
           contact_age == "40-59y"~59,
           contact_age == "60+y"~99,
           contact_age== "I don't know" ~ NA_real_,
           is.na(contact_age) ~ NA_real_
         )
  ) 

### One urban and one rural
cnt_u <- contacts %>% 
  filter(study_site == "Urban")

cnt_r <- contacts %>% 
  filter(study_site == "Rural")


df_r <- survey(
  participants = cnt_r %>% 
    select(part_id, part_age, part_weight) %>% 
    unique() %>%
    rename(weights = part_weight) %>%
    mutate(country = rep("Mozambique"),
           year = rep(2020)),
  
  contacts = cnt_r %>%
    select(part_id,cnt_age_est_min, cnt_age_est_max, cnt_home:cnt_otherplace)
  
)

df_u <- survey(
  participants = cnt_u %>% 
    select(part_id, part_age,part_weight) %>% 
    unique()%>%
    rename(weights = part_weight) %>%
    mutate(country=rep("Mozambique"),
           year=rep(2020)),
  
  contacts = cnt_u %>%
    select(part_id,cnt_age_est_min, cnt_age_est_max, cnt_home:cnt_otherplace)
  
)


##### Creating matrices

### urban
m_u <- contact_matrix(df_u, 
                      age.limits = c(0,10,20,30,40,50,60,99), ##Specify age bands for the matrix
                      symmetric=T,  ##symmetric matrix
                      missing.contact.age="sample", 
                      estimated.contact.age = "sample",  ##what to do if missing contact age group, use sample with bootstraping
                      weigh.age = T,
                      return.part.weights=T,
                      n=1000)     ##Number of bootstraps

#Code to take the mean of matrices
mr_u <- Reduce("+", lapply(m_u$matrices, function(x) {x$matrix})) / length(m_u$matrices)

#Make into long form
mat_u <- reshape2::melt(mr_u, varnames = c("age1", "age_cont"), value.name = "contacts") %>%
  left_join(data.frame(age1 = c(1,2,3,4,5,6,7),
                       age_part = c("[0,10)","[10,20)","[20,30)","[30,40)","[40,50)","[50,60)","60+")),
            by="age1")

### Do same for rural
m_r <- contact_matrix(df_r, age.limits = c(0,10,20,30,40,50,60,99),symmetric=T, 
                      missing.contact.age="sample",estimated.contact.age = "sample",
                      return.part.weights = T,n=1000)


mr_r <- Reduce("+", lapply(m_r$matrices, function(x) {x$matrix})) / length(m_r$matrices)
mat_r <- reshape2::melt(mr_r, varnames = c("age1", "age_cont"), value.name = "contacts") %>%
  left_join(data.frame(age1 = c(1,2,3,4,5,6,7),
                       age_part = c("[0,10)","[10,20)","[20,30)","[30,40)","[40,50)","[50,60)","60+")),
            by="age1")


##### Matrix visualization

### urban
ggplot(mat_u, aes(x = age_part, y = age_cont, fill = contacts)) + 
  theme(legend.position = "bottom") + 
  scale_fill_gradient2(low = "white", high = "#273871", mid = "#7FABD3", midpoint = 3.4, limit = c(0,7))+
  xlab("Age of participant")+ylab("Age of contact")+
  geom_tile()+
  geom_text(aes(label=round(contacts, digits=2)), colour = "black", check_overlap = TRUE)+
  theme()

### rural
ggplot(mat_r, aes(x = age_part, y = age_cont, fill = contacts)) + 
  theme(legend.position = "bottom") + 
  scale_fill_gradient2(low = "white", high = "#273871", mid = "#7FABD3", midpoint = 3.4, limit = c(0,7))+
  xlab("Age of participant")+ylab("Age of contact")+
  geom_tile()+ 
  geom_text(aes(label=round(contacts, digits=2)), colour = "black", check_overlap = TRUE)+
  theme()




#####          CODE FOR TRANSMISSION MODEL           #####

### Fix R0. 2.5 to get q parameter

# function
getr0 <- function(q,CM,d){
  m_ngm <- CM*q*d
  r0 <- max(eigen(as.matrix(m_ngm))$values)
  print(r0)
}

# globalmix rural matrix
rural <- c(5.91, 5.31, 2.72, 3.17, 2.91, 3.86, 3.11,
           3.91, 13.77, 5.38, 4.91, 4.80, 5.36, 3.44,
           1.37, 3.68, 3.27, 2.58, 2.33, 3.06, 1.81,
           1.10, 2.31, 1.78, 4.04, 3.27, 3.99, 2.58,
           0.64, 1.44, 1.02, 2.08, 2.75, 3.36, 1.76,
           0.55, 1.05, 0.88, 1.65, 2.19, 2.99, 2.01,
           0.46, 0.70, 0.54, 1.11, 1.19, 2.09, 1.84)
cm_rural <- matrix(rural, nrow = 7, ncol = 7)
getr0(q=0.01676074, CM=cm_rural, d=7)

# globalmix urban matrix
urban <- c(4.46, 3.05, 3.05, 2.91, 1.04, 2.01, 1.94,
           2.24, 9.61, 3.70, 3.76, 3.22, 3.01, 2.08,
           1.53, 2.53, 1.99, 2.26, 1.88, 2.93, 1.36,
           1.01, 1.77, 1.56, 3.48, 2.01, 2.38, 1.47,
           0.23, 0.97, 0.82, 1.28, 1.55, 2.17, 1.12,
           0.29, 0.59, 0.84, 0.99, 1.41, 1.78, 0.87,
           0.29, 0.42, 0.41, 0.64, 0.76, 0.90, 1.53)
cm_urban <- matrix(urban, nrow = 7, ncol = 7)
getr0(q=0.024248775, CM=cm_urban, d=7)


# prem 
prem <- c(19.34, 4.54, 1.68, 2.63, 1.76, 1.45, 1.02,
          3.66, 14.09, 2.86, 1.90, 2.33, 1.92, 1.10, 
          2.33, 2.19, 6.10, 2.67, 1.90, 1.80, 0.56,
          2.67, 1.58, 2.81, 3.63, 2.62, 2.10, 0.75, 
          1.08, 1.31, 1.76, 2.41, 2.69, 2.46, 0.70, 
          0.56, 0.46, 1.04, 1.21, 1.38, 1.78, 0.63, 
          0.39, 0.22, 0.28, 0.32, 0.31, 0.44, 0.33)
cm_prem <- matrix(prem, nrow = 7, ncol = 7)
getr0(q=0.01521068, CM=cm_prem, d=7)



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
    lambda.1 <- q*c11*(i.num.1/num.1) + 
      q*c12*(i.num.2/num.2) + 
      q*c13*(i.num.3/num.3) + 
      q*c14*(i.num.4/num.4) +
      q*c15*(i.num.5/num.5) + 
      q*c16*(i.num.6/num.6) + 
      q*c17*(i.num.7/num.7)
    
    lambda.v.1 <- (1-psi)*q*c11*(i.num.1/num.1) + 
      (1-psi)*q*c12*(i.num.2/num.2) + 
      (1-psi)*q*c13*(i.num.3/num.3) + 
      (1-psi)*q*c14*(i.num.4/num.4) +
      (1-psi)*q*c15*(i.num.5/num.5) + 
      (1-psi)*q*c16*(i.num.6/num.6) + 
      (1-psi)*q*c17*(i.num.7/num.7)
    
    # force of infection for 10-19 contacts (infecting the 10-19 year olds, infecting the contact)
    lambda.2 <- q*c21*(i.num.1/num.1) + 
      q*c22*(i.num.2/num.2) + 
      q*c23*(i.num.3/num.3) + 
      q*c24*(i.num.4/num.4) +
      q*c25*(i.num.5/num.5) + 
      q*c26*(i.num.6/num.6) + 
      
    lambda.v.2 <- (1-psi)*q*c21*(i.num.1/num.1) + 
      (1-psi)*q*c22*(i.num.2/num.2) + 
      (1-psi)*q*c23*(i.num.3/num.3) + 
      (1-psi)*q*c24*(i.num.4/num.4) +
      (1-psi)*q*c25*(i.num.5/num.5) + 
      (1-psi)*q*c26*(i.num.6/num.6) + 
      (1-psi)*q*c27*(i.num.7/num.7)
    
    # force of infection for 20-29 contacts
    lambda.3 <- q*c31*(i.num.1/num.1) + 
      q*c32*(i.num.2/num.2) + 
      q*c33*(i.num.3/num.3) + 
      q*c34*(i.num.4/num.4) +
      q*c35*(i.num.5/num.5) + 
      q*c36*(i.num.6/num.6) + 
      q*c37*(i.num.7/num.7)
    
    lambda.v.3 <- (1-psi)*q*c31*(i.num.1/num.1) + 
      (1-psi)*q*c32*(i.num.2/num.2) + 
      (1-psi)*q*c33*(i.num.3/num.3) + 
      (1-psi)*q*c34*(i.num.4/num.4) +
      (1-psi)*q*c35*(i.num.5/num.5) + 
      (1-psi)*q*c36*(i.num.6/num.6) + 
      (1-psi)*q*c37*(i.num.7/num.7)
    
    # force of infection for 30-39 contacts
    lambda.4 <- q*c41*(i.num.1/num.1) + 
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
    lambda.5 <- q*c51*(i.num.1/num.1) + 
      q*c52*(i.num.2/num.2) + 
      q*c53*(i.num.3/num.3) + 
      q*c54*(i.num.4/num.4) +
      q*c55*(i.num.5/num.5) + 
      q*c56*(i.num.6/num.6) + 
      q*c57*(i.num.7/num.7)
    
    lambda.v.5 <- (1-psi)*q*c51*(i.num.1/num.1) + 
      (1-psi)*q*c52*(i.num.2/num.2) + 
      (1-psi)*q*c53*(i.num.3/num.3) + 
      (1-psi)*q*c54*(i.num.4/num.4) +
      (1-psi)*q*c55*(i.num.5/num.5) + 
      (1-psi)*q*c56*(i.num.6/num.6) + 
      (1-psi)*q*c57*(i.num.7/num.7)
    
    # force of infection for 50-59 contacts
    lambda.6 <- q*c61*(i.num.1/num.1) + 
      q*c62*(i.num.2/num.2) + 
      q*c63*(i.num.3/num.3) + 
      q*c64*(i.num.4/num.4) +
      q*c65*(i.num.5/num.5) + 
      q*c66*(i.num.6/num.6) + 
      q*c67*(i.num.7/num.7)
    
    lambda.v.6 <- (1-psi)*q*c61*(i.num.1/num.1) + 
      (1-psi)*q*c62*(i.num.2/num.2) + 
      (1-psi)*q*c63*(i.num.3/num.3) + 
      (1-psi)*q*c64*(i.num.4/num.4) +
      (1-psi)*q*c65*(i.num.5/num.5) + 
      (1-psi)*q*c66*(i.num.6/num.6) + 
      (1-psi)*q*c67*(i.num.7/num.7)
    
    # force of infection for 60+ contacts
    lambda.7 <- q*c71*(i.num.1/num.1) + 
      q*c72*(i.num.2/num.2) + 
      q*c73*(i.num.3/num.3) + 
      q*c74*(i.num.4/num.4) +
      q*c75*(i.num.5/num.5) + 
      q*c76*(i.num.6/num.6) + 
      q*c77*(i.num.7/num.7)
    
    lambda.v.7 <- (1-psi)*q*c71*(i.num.1/num.1) + 
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
           svi.flow.1 = lambda.v.1*s.v.num.1, svi.flow.2 = lambda.v.2*s.v.num.2,
           svi.flow.3 = lambda.v.3*s.v.num.3, svi.flow.4 = lambda.v.4*s.v.num.4, 
           svi.flow.5 = lambda.v.5*s.v.num.5, svi.flow.6 = lambda.v.6*s.v.num.6,
           svi.flow.7 = lambda.v.7*s.v.num.7
    ))
  })
}

### Parameters
# rural contact patterns
param.rural <- param.dcm(gamma = 1/7, psi = 0.50, q = 0.01676074,
                         c11=5.91, c12=5.31, c13=2.72, c14=3.17, c15=2.91, c16=3.86, c17=3.11,
                         c21=3.91, c22=13.77, c23=5.38, c24=4.91, c25=4.80, c26=5.36, c27=3.44,
                         c31=1.37, c32=3.68, c33=3.27, c34=2.58, c35=2.33, c36=3.06, c37=1.81,
                         c41=1.10, c42=2.31, c43=1.78, c44=4.04, c45=3.27, c46=3.99, c47=2.58,
                         c51=0.64, c52=1.44, c53=1.02, c54=2.08, c55=2.75, c56=3.36, c57=1.76,
                         c61=0.55, c62=1.05, c63=0.88, c64=1.65, c65=2.19, c66=2.99, c67=2.01,
                         c71=0.46, c72=0.70, c73=0.54, c74=1.11, c75=1.19, c76=2.09, c77=1.84)

# urban contact patterns
param.urban <- param.dcm(gamma = 1/7, psi = 0.50, q = 0.024248775,
                         c11=4.46, c12=3.05, c13=3.05, c14=2.91, c15=1.04, c16=2.01, c17=1.94,
                         c21=2.24, c22=9.61, c23=3.70, c24=3.76, c25=3.22, c26=3.01, c27=2.08,
                         c31=1.53, c32=2.53, c33=1.99, c34=2.26, c35=1.88, c36=2.93, c37=1.36,
                         c41=1.01, c42=1.77, c43=1.56, c44=3.48, c45=2.01, c46=2.38, c47=1.47,
                         c51=0.23, c52=0.97, c53=0.82, c54=1.28, c55=1.55, c56=2.17, c57=1.12,
                         c61=0.29, c62=0.59, c63=0.84, c64=0.99, c65=1.41, c66=1.78, c67=0.87,
                         c71=0.29, c72=0.42, c73=0.41, c74=0.64, c75=0.76, c76=0.90, c77=1.53)

# prem et al contact patterns
param.prem <- param.dcm(gamma = 1/7, psi = 0.50, q = 0.01521068,
                        c11=19.34, c12=4.54, c13=1.68, c14=2.63, c15=1.76, c16=1.45, c17=1.02,
                        c21=3.66, c22=14.09, c23=2.86, c24=1.9, c25=2.33, c26=1.92, c27=1.1,
                        c31=2.33, c32=2.19, c33=6.1, c34=2.67, c35=1.9, c36=1.8, c37=0.56,
                        c41=2.67, c42=1.58, c43=2.81, c44=3.63, c45=2.62, c46=2.1, c47=0.75,
                        c51=1.08, c52=1.31, c53=1.76, c54=2.41, c55=2.69, c56=2.46, c57=0.7,
                        c61=0.56, c62=0.46, c63=1.04, c64=1.21, c65=1.38, c66=1.78, c67=0.63,
                        c71=0.39, c72=0.22, c73=0.28, c74=0.32, c75=0.31, c76=0.44, c77=0.33)


### Initial conditions
# rural with vaccine
init.vax.rur <- init.dcm(s.num.1 = 0.50*6782044, s.v.num.1 = 0.50*6782044, i.num.1 = 1, r.num.1 = 0,
                         s.num.2 = 0.50*5090656, s.v.num.2 = 0.50*5090656, i.num.2 = 1, r.num.2 = 0,
                         s.num.3 = 0.50*3120038, s.v.num.3 = 0.50*3120038, i.num.3 = 1, r.num.3 = 0,
                         s.num.4 = 0.50*1917767, s.v.num.4 = 0.50*1917767, i.num.4 = 1, r.num.4 = 0,
                         s.num.5 = 0.50*1461670, s.v.num.5 = 0.50*1461670, i.num.5 = 1, r.num.5 = 0,
                         s.num.6 = 0.50*887489, s.v.num.6 = 0.50*887489, i.num.6 = 1, r.num.6 = 0,
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
                           s.num.5 = 1461670, s.v.num.5 = 0, i.num.5 = 1, r.num.5 = 0,
                           s.num.6 = 887489, s.v.num.6 = 0,  i.num.6 = 1, r.num.6 = 0,
                           s.num.7 = 1013562, s.v.num.7 = 0, i.num.7 = 1, r.num.7 = 0,
                           si.flow.1 = 0, si.flow.2 = 0, si.flow.3 = 0, si.flow.4 = 0,
                           si.flow.5 = 0, si.flow.6 = 0, si.flow.7 = 0, 
                           svi.flow.1 = 0, svi.flow.2 = 0, svi.flow.3 = 0, svi.flow.4 = 0,
                           svi.flow.5 = 0, svi.flow.6 = 0, svi.flow.7 = 0)

# urban with vaccine
init.vax.urb <- init.dcm(s.num.1 = 0.50*2884776, s.v.num.1 = 0.50*2884776, i.num.1 = 1, r.num.1 = 0,
                         s.num.2 = 0.50*2589432, s.v.num.2 = 0.50*2589432, i.num.2 = 1, r.num.2 = 0,
                         s.num.3 = 0.50*2024202, s.v.num.3 = 0.50*2024202, i.num.3 = 1, r.num.3 = 0,
                         s.num.4 = 0.50*1317494, s.v.num.4 = 0.50*1317494, i.num.4 = 1, r.num.4 = 0,
                         s.num.5 = 0.50*805883, s.v.num.5 = 0.50*805883, i.num.5 = 1, r.num.5 = 0,
                         s.num.6 = 0.50*469481, s.v.num.6 = 0.50*469481, i.num.6 = 1, r.num.6 = 0,
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
                           s.num.5 = 805883, s.v.num.5 = 0, i.num.5 = 1, r.num.5 = 0,
                           s.num.6 = 469481, s.v.num.6 = 0, i.num.6 = 1, r.num.6 = 0,
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
                          s.num.5 = 0.50*2267553, s.v.num.5 = 0.50*2267553, i.num.5 = 1, r.num.5 = 0,
                          s.num.6 = 0.50*1356970, s.v.num.6 = 0.50*1356970, i.num.6 = 1, r.num.6 = 0,
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
                            s.num.5 = 2267553, s.v.num.5 = 0, i.num.5 = 1, r.num.5 = 0,
                            s.num.6 = 1356970, s.v.num.6 = 0, i.num.6 = 1, r.num.6 = 0,
                            s.num.7 = 1481312, s.v.num.7 = 0, i.num.7 = 1, r.num.7 = 0,
                            si.flow.1 = 0, si.flow.2 = 0, si.flow.3 = 0, si.flow.4 = 0, si.flow.5 = 0,
                            si.flow.6 = 0, si.flow.7 = 0,
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


### age group 60+
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
