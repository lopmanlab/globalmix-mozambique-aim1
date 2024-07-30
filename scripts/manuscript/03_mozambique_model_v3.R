# ------------------------------------------------------------------------------
# This file contains scripts to analyze and visualize age-structured contacts
# Author: Sara Kim
# Date: 08/18/2023; 
# modified 03/13/2024 - 2021 Prem et al matrices stratified by rural/urban
# ------------------------------------------------------------------------------


#---------- Prem Rural Matrix ----------#
moz_prem_rural <- read.csv("../../data/clean/moz_prem_2021_rural.csv")

### Generate data frame of participant age groups for orig prem matrix matched to age groups for target matrix
age_cat_part <- data.frame(part_age1=c("0_4","5_9","10_14","15_19","20_24","25_29",
                                       "30_34","35_39", "40_44","45_49","50_54",
                                       "55_59","60_64","65_69","70_74","75"),
                           
                           part_age2 =c("0-9y", "0-9y", "10-19y","10-19y", 
                                        "20-29y", "20-29y", "30-39y", "30-39y", 
                                        "40-49y","40-49y", "50-59y", "50-59y",  
                                        "60+y", "60+y", "60+y", "60+y"))


### Generate data frame of contact age groups for orig prem matrix matched to contact age groups for target matrix
age_cat_cont <- data.frame(cont_age1 = colnames(moz_prem_rural),
                           cont_age2 = c("0-9y", "0-9y", "10-19y","10-19y", 
                                         "20-29y", "20-29y", "30-39y", "30-39y", 
                                         "40-49y","40-49y", "50-59y", "50-59y",  
                                         "60+y", "60+y", "60+y", "60+y"))

moz_prem_rural$part_age1 <- age_cat_part$part_age1


### pivot_longer for ease of data manipulation
moz_prem_rural <- moz_prem_rural %>%
  pivot_longer(cols = contact_0_4:contact_75, 
               names_to = "cont_age",
               values_to = "avg_cont")

### Create a column for the age groups of the target matrix
moz_prem_rural <- moz_prem_rural %>%
  left_join(age_cat_part, by = c("part_age1" = "part_age1")) %>%
  left_join(age_cat_cont, by = c("cont_age" = "cont_age1"))


### population data for 5-year population distribution in Moz
moz_pop_5yr_rural <- read.csv("../../data/clean/moz_agecat_5yr_rural.csv")

### population data for 10-year population distribution in Moz
moz_pop_10yr_rural <- read.csv("../../data/clean/moz_agecat_10yr_rural.csv") %>%
  # recategorize ages to conform to original agecat_10_total part_age2 groups
  mutate(part_age2 = case_when(part_age2 == "0_9" ~ "0-9y", 
                               part_age2 == "10_19" ~ "10-19y", 
                               part_age2 == "20_29" ~ "20-29y",
                               part_age2 == "30_39" ~ "30-39y", 
                               part_age2 == "40_49" ~  "40-49y",
                               part_age2 == "50_59" ~  "50-59y",
                               part_age2 == "60" ~ "60+y")) %>%
  group_by(part_age2) %>%
  dplyr::summarize(pop10yr = sum(pop10yr))


### "linearly" collapse the participant-contact cells based on population sizes
moz_prem10_rural <- moz_prem_rural %>%
  left_join(moz_pop_5yr_rural, by = c("part_age1" = "part_age1")) %>%  ## pop size of 5-year participant age band
  mutate(tot_pop_contacts = avg_cont * pop5yr) %>%   ## total contacts made between 0_4 & 0_4, 0_4&5_9 etc
  group_by(part_age2, cont_age2) %>%
  dplyr::summarize(tot_pop_contacts = sum(tot_pop_contacts)) %>% ##total contacts made between 0_9&0_9, 0_9&10_19 etc
  left_join(moz_pop_10yr_rural, by = c("part_age2"="part_age2")) %>% # pop size of 10-year participant age band
  mutate(contacts = tot_pop_contacts/pop10yr)        ## average contacts made using 10-year age band

### plot
premmatrix_rural <- ggplot(moz_prem10_rural, aes(x = part_age2, y = cont_age2, fill = contacts)) + 
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


#---------- Prem Urban Matrix ----------#
moz_prem_urban <- read.csv("../../data/clean/moz_prem_2021_urban.csv")

### Generate data frame of participant age groups for orig prem matrix matched to age groups for target matrix
age_cat_part <- data.frame(part_age1=c("0_4","5_9","10_14","15_19","20_24","25_29",
                                       "30_34","35_39", "40_44","45_49","50_54",
                                       "55_59","60_64","65_69","70_74","75"),
                           
                           part_age2 =c("0-9y", "0-9y", "10-19y","10-19y", 
                                        "20-29y", "20-29y", "30-39y", "30-39y", 
                                        "40-49y","40-49y", "50-59y", "50-59y",  
                                        "60+y", "60+y", "60+y", "60+y"))


### Generate data frame of contact age groups for orig prem matrix matched to contact age groups for target matrix
age_cat_cont <- data.frame(cont_age1 = colnames(moz_prem_urban),
                           cont_age2 = c("0-9y", "0-9y", "10-19y","10-19y", 
                                         "20-29y", "20-29y", "30-39y", "30-39y", 
                                         "40-49y","40-49y", "50-59y", "50-59y",  
                                         "60+y", "60+y", "60+y", "60+y"))

moz_prem_urban$part_age1 <- age_cat_part$part_age1


### pivot_longer for ease of data manipulation
moz_prem_urban <- moz_prem_urban %>%
  pivot_longer(cols = contact_0_4:contact_75, 
               names_to = "cont_age",
               values_to = "avg_cont")

### Create a column for the age groups of the target matrix
moz_prem_urban <- moz_prem_urban %>%
  left_join(age_cat_part, by = c("part_age1" = "part_age1")) %>%
  left_join(age_cat_cont, by = c("cont_age" = "cont_age1"))


### population data for 5-year population distribution in Moz
moz_pop_5yr_urban <- read.csv("../../data/clean/moz_agecat_5yr_urban.csv")

### population data for 10-year population distribution in Moz
moz_pop_10yr_urban <- read.csv("../../data/clean/moz_agecat_10yr_urban.csv") %>%
  # recategorize ages to conform to original agecat_10_total part_age2 groups
  mutate(part_age2 = case_when(part_age2 == "0_9" ~ "0-9y", 
                               part_age2 == "10_19" ~ "10-19y", 
                               part_age2 == "20_29" ~ "20-29y",
                               part_age2 == "30_39" ~ "30-39y", 
                               part_age2 == "40_49" ~  "40-49y",
                               part_age2 == "50_59" ~  "50-59y",
                               part_age2 == "60" ~ "60+y")) %>%
  group_by(part_age2) %>%
  dplyr::summarize(pop10yr = sum(pop10yr))


### "linearly" collapse the participant-contact cells based on population sizes
moz_prem10_urban <- moz_prem_urban %>%
  left_join(moz_pop_5yr_urban, by = c("part_age1" = "part_age1")) %>%  ## pop size of 5-year participant age band
  mutate(tot_pop_contacts = avg_cont * pop5yr) %>%   ## total contacts made between 0_4 & 0_4, 0_4&5_9 etc
  group_by(part_age2, cont_age2) %>%
  dplyr::summarize(tot_pop_contacts = sum(tot_pop_contacts)) %>% ##total contacts made between 0_9&0_9, 0_9&10_19 etc
  left_join(moz_pop_10yr_urban, by = c("part_age2"="part_age2")) %>% # pop size of 10-year participant age band
  mutate(contacts = tot_pop_contacts/pop10yr)        ## average contacts made using 10-year age band

### plot
premmatrix_urban <- ggplot(moz_prem10_urban, aes(x = part_age2, y = cont_age2, fill = contacts)) + 
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


# ---------- Transmission Model ---------- #

### Fix R0. 2.5 to get q parameter
# function
getr0 <- function(q, CM, d){
  m_ngm <- CM * q * d
  r0 <- max(eigen(as.matrix(m_ngm))$values)
  print(r0)
}

# Prem 2021 rural values 
prem_rural <- c(8.1,3.7,2.5,4.0,2.6,2.3,2.0,
                2.5,10.9,3.0,2.3,2.8,2.2,1.5,
                1.7,1.8,5.6,2.9,2.4,2.7,1.3,
                1.9,1.4,2.2,3.4,2.8,2.3,1.4,
                1.0,1.2,1.4,2.1,2.6,2.2,1.1,
                0.7,0.6,0.9,1.2,1.4,1.7,0.8,
                0.7,0.7,0.8,0.9,1.0,1.1,1.0)
cm_prem_rural <- matrix(prem_rural, nrow = 7, ncol = 7)
getr0(q=0.020862535, CM=cm_prem_rural, d=7)

# Prem 2021 urban values 
prem_urban <- c(9.9,2.5,2.0,3.3,2.3,1.8,1.7,
                3.0,17.2,3.9,2.9,3.6,2.9,1.8,
                2.0,2.1,6.5,3.5,3.0,3.2,1.5,
                1.8,1.3,1.9,3.0,2.5,2.1,1.5,
                1.0,1.1,1.1,1.6,2.0,1.7,1.1,
                0.6,0.5,0.6,0.7,0.7,0.9,0.7,
                0.7,0.6,0.6,0.7,0.7,0.8,0.9)
cm_prem_urban <- matrix(prem_urban, nrow = 7, ncol = 7)
getr0(q=0.017250615, CM=cm_prem_urban, d=7)


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


# prem et al contact patterns
param.prem.rural <- param.dcm(gamma = 1/7, psi = 0.50, q = 0.020862535,
                              c11=8.1, c12=3.7, c13=2.5, c14=4.0, c15=2.6, c16=2.3, c17=2.0,
                              c21=2.5, c22=10.9, c23=3.0, c24=2.3, c25=2.8, c26=2.2, c27=1.5,
                              c31=1.7, c32=1.8, c33=5.6, c34=2.9, c35=2.4, c36=2.7, c37=1.3,
                              c41=1.9, c42=1.4, c43=2.2, c44=3.4, c45=2.8, c46=2.3, c47=1.4,
                              c51=1.0, c52=1.2, c53=1.4, c54=2.1, c55=2.6, c56=2.2, c57=1.1,
                              c61=0.7, c62=0.6, c63=0.9, c64=1.2, c65=1.4, c66=1.7, c67=0.8,
                              c71=0.7, c72=0.7, c73=0.8, c74=0.9, c75=1.0, c76=1.1, c77=1.0)

param.prem.urban <- param.dcm(gamma = 1/7, psi = 0.50, q = 0.017250615,
                              c11=9.9, c12=3.5, c13=2.0, c14=3.3, c15=2.3, c16=1.8, c17=1.7,
                              c21=3.0, c22=17.2, c23=3.9, c24=2.9, c25=3.6, c26=2.9, c27=1.8,
                              c31=2.0, c32=2.1, c33=6.5, c34=3.5, c35=3.0, c36=3.2, c37=1.5,
                              c41=1.8, c42=1.3, c43=1.9, c44=3.0, c45=2.5, c46=2.1, c47=1.5,
                              c51=1.0, c52=1.1, c53=1.1, c54=1.6, c55=2.0, c56=1.7, c57=1.1,
                              c61=0.6, c62=0.5, c63=0.6, c64=0.7, c65=0.7, c66=0.9, c67=0.7,
                              c71=0.7, c72=0.6, c73=0.6, c74=0.7, c75=0.7, c76=0.8, c77=0.9)

### Initial conditions
# prem rural with vaccine
init.vax.prem.rural <- init.dcm(s.num.1 = 0.50*6782044, s.v.num.1 = 0.50*6782044, i.num.1 = 1, r.num.1 = 0,
                                s.num.2 = 0.50*5090656, s.v.num.2 = 0.50*5090656, i.num.2 = 1, r.num.2 = 0,
                                s.num.3 = 0.50*3120038, s.v.num.3 = 0.50*3120038, i.num.3 = 1, r.num.3 = 0,
                                s.num.4 = 0.50*1917767, s.v.num.4 = 0.50*1917767, i.num.4 = 1, r.num.4 = 0,
                                s.num.5 = 0.50*1461670, s.v.num.5 = 0.50*1461670, i.num.5 = 1, r.num.5 = 0,
                                s.num.6 = 0.50*887489, s.v.num.6 = 0.50*887489, i.num.6 = 1, r.num.6 = 0,
                                s.num.7 = 0.50*1013562, s.v.num.7 = 0.50*1013562, i.num.7 = 1, r.num.7 = 0,
                                si.flow.1 = 0, si.flow.2 = 0, si.flow.3 = 0, si.flow.4 = 0, si.flow.5 = 0,
                                si.flow.6 = 0, si.flow.7 = 0,
                                svi.flow.1 = 0, svi.flow.2 = 0, svi.flow.3 = 0, svi.flow.4 = 0,
                                svi.flow.5 = 0, svi.flow.6 = 0, svi.flow.7 = 0)

# prem rural with no vaccine
init.novax.prem.rural <- init.dcm(s.num.1 = 6782044, s.v.num.1 = 0, i.num.1 = 1, r.num.1 = 0,
                                  s.num.2 = 5090656, s.v.num.2 = 0, i.num.2 = 1, r.num.2 = 0,
                                  s.num.3 = 3120038, s.v.num.3 = 0, i.num.3 = 1, r.num.3 = 0,
                                  s.num.4 = 1917767, s.v.num.4 = 0, i.num.4 = 1, r.num.4 = 0,
                                  s.num.5 = 1461670, s.v.num.5 = 0, i.num.5 = 1, r.num.5 = 0,
                                  s.num.6 = 887489, s.v.num.6 = 0, i.num.6 = 1, r.num.6 = 0,
                                  s.num.7 = 1013562, s.v.num.7 = 0, i.num.7 = 1, r.num.7 = 0,
                                  si.flow.1 = 0, si.flow.2 = 0, si.flow.3 = 0, si.flow.4 = 0, 
                                  si.flow.5 = 0, si.flow.6 = 0, si.flow.7 = 0,
                                  svi.flow.1 = 0, svi.flow.2 = 0, svi.flow.3 = 0, svi.flow.4 = 0,
                                  svi.flow.5 = 0, svi.flow.6 = 0, svi.flow.7 = 0)

# prem urban with vaccine
init.vax.prem.urban <- init.dcm(s.num.1 = 0.50*2884776, s.v.num.1 = 0.50*2884776, i.num.1 = 1, r.num.1 = 0,
                                s.num.2 = 0.50*2589432, s.v.num.2 = 0.50*2589432, i.num.2 = 1, r.num.2 = 0,
                                s.num.3 = 0.50*2024202, s.v.num.3 = 0.50*2024202, i.num.3 = 1, r.num.3 = 0,
                                s.num.4 = 0.50*1317494, s.v.num.4 = 0.50*1317494, i.num.4 = 1, r.num.4 = 0,
                                s.num.5 = 0.50*805883, s.v.num.5 = 0.50*805883, i.num.5 = 1, r.num.5 = 0,
                                s.num.6 = 0.50*469481, s.v.num.6 = 0.50*469481, i.num.6 = 1, r.num.6 = 0,
                                s.num.7 = 0.50*467750, s.v.num.7 = 0.50*467750, i.num.7 = 1, r.num.7 = 0,
                                si.flow.1 = 0, si.flow.2 = 0, si.flow.3 = 0, si.flow.4 = 0, si.flow.5 = 0,
                                si.flow.6 = 0, si.flow.7 = 0,
                                svi.flow.1 = 0, svi.flow.2 = 0, svi.flow.3 = 0, svi.flow.4 = 0,
                                svi.flow.5 = 0, svi.flow.6 = 0, svi.flow.7 = 0)

# prem rural with no vaccine
init.novax.prem.urban <- init.dcm(s.num.1 = 2884776, s.v.num.1 = 0, i.num.1 = 1, r.num.1 = 0,
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

### Controls 
control <- control.dcm(nstep=360, new.mod = seirmod)

### Compile model Prem et al
sim.vax.prem.rural <- dcm(param.prem.rural, init.vax.prem.rural, control)
df.vax.prem.rural <- as.data.frame(sim.vax.prem.rural)
sim.novax.prem.rural <- dcm(param.prem.rural, init.novax.prem.rural, control)
df.novax.prem.rural <- as.data.frame(sim.novax.prem.rural)

sim.vax.prem.urban <- dcm(param.prem.urban, init.vax.prem.urban, control)
df.vax.prem.urban <- as.data.frame(sim.vax.prem.urban)
sim.novax.prem.urban <- dcm(param.prem.urban, init.novax.prem.urban, control)
df.novax.prem.urban <- as.data.frame(sim.novax.prem.urban)



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
AR.prem.rural.0_9 <- print(AR(df.novax.prem.rural$si.flow.1, init.novax.prem.rural$s.num.1)) 
ARv.prem.rural.0_9 <- print(ARv(df.vax.prem.rural$si.flow.1, init.novax.prem.rural$s.num.1)) 
ov.eff.prem.rural.0_9 <- print(overall.eff(df.vax.prem.rural$si.flow.1, df.novax.prem.rural$si.flow.1)) 

AR.prem.urban.0_9 <- print(AR(df.novax.prem.urban$si.flow.1, init.novax.prem.urban$s.num.1)) 
ARv.prem.urban.0_9 <- print(ARv(df.vax.prem.urban$si.flow.1, init.novax.prem.urban$s.num.1)) 
ov.eff.prem.urban.0_9 <- print(overall.eff(df.vax.prem.urban$si.flow.1, df.novax.prem.urban$si.flow.1)) 

### age group 10-19
AR.prem.rural.10_19 <- print(AR(df.novax.prem.rural$si.flow.2, init.novax.prem.rural$s.num.2)) 
ARv.prem.rural.10_19 <- print(ARv(df.vax.prem.rural$si.flow.2, init.novax.prem.rural$s.num.2)) 
ov.eff.prem.rural.10_19 <- print(overall.eff(df.vax.prem.rural$si.flow.2, df.novax.prem.rural$si.flow.2)) 

AR.prem.urban.10_19 <- print(AR(df.novax.prem.urban$si.flow.2, init.novax.prem.urban$s.num.2)) 
ARv.prem.urban.10_19 <- print(ARv(df.vax.prem.urban$si.flow.2, init.novax.prem.urban$s.num.2)) 
ov.eff.prem.urban.10_19 <- print(overall.eff(df.vax.prem.urban$si.flow.2, df.novax.prem.urban$si.flow.2)) 

### age group 20-29
AR.prem.rural.20_29 <- print(AR(df.novax.prem.rural$si.flow.3, init.novax.prem.rural$s.num.3)) 
ARv.prem.rural.20_29 <- print(ARv(df.vax.prem.rural$si.flow.3, init.novax.prem.rural$s.num.3)) 
ov.eff.prem.rural.20_29 <- print(overall.eff(df.vax.prem.rural$si.flow.3, df.novax.prem.rural$si.flow.3)) 

AR.prem.urban.20_29 <- print(AR(df.novax.prem.urban$si.flow.3, init.novax.prem.urban$s.num.3)) 
ARv.prem.urban.20_29 <- print(ARv(df.vax.prem.urban$si.flow.3, init.novax.prem.urban$s.num.3)) 
ov.eff.prem.urban.20_29 <- print(overall.eff(df.vax.prem.urban$si.flow.3, df.novax.prem.urban$si.flow.3)) 

### age group 30-39
AR.prem.rural.30_39 <- print(AR(df.novax.prem.rural$si.flow.4, init.novax.prem.rural$s.num.4)) 
ARv.prem.rural.30_39 <- print(ARv(df.vax.prem.rural$si.flow.4, init.novax.prem.rural$s.num.4)) 
ov.eff.prem.rural.30_39 <- print(overall.eff(df.vax.prem.rural$si.flow.4, df.novax.prem.rural$si.flow.4)) 

AR.prem.urban.30_39 <- print(AR(df.novax.prem.urban$si.flow.4, init.novax.prem.urban$s.num.4)) 
ARv.prem.urban.30_39 <- print(ARv(df.vax.prem.urban$si.flow.4, init.novax.prem.urban$s.num.4)) 
ov.eff.prem.urban.30_39 <- print(overall.eff(df.vax.prem.urban$si.flow.4, df.novax.prem.urban$si.flow.4)) 


### age group 40-49
AR.prem.rural.40_49 <- print(AR(df.novax.prem.rural$si.flow.5, init.novax.prem.rural$s.num.5)) 
ARv.prem.rural.40_49 <- print(ARv(df.vax.prem.rural$si.flow.5, init.novax.prem.rural$s.num.5)) 
ov.eff.prem.rural.40_49 <- print(overall.eff(df.vax.prem.rural$si.flow.5, df.novax.prem.rural$si.flow.5)) 

AR.prem.urban.40_49 <- print(AR(df.novax.prem.urban$si.flow.5, init.novax.prem.urban$s.num.5)) 
ARv.prem.urban.40_49 <- print(ARv(df.vax.prem.urban$si.flow.5, init.novax.prem.urban$s.num.5)) 
ov.eff.prem.urban.40_49 <- print(overall.eff(df.vax.prem.urban$si.flow.5, df.novax.prem.urban$si.flow.5)) 

### age group 50-59
AR.prem.rural.50_59 <- print(AR(df.novax.prem.rural$si.flow.6, init.novax.prem.rural$s.num.6)) 
ARv.prem.rural.50_59 <- print(ARv(df.vax.prem.rural$si.flow.6, init.novax.prem.rural$s.num.6)) 
ov.eff.prem.rural.50_59 <- print(overall.eff(df.vax.prem.rural$si.flow.6, df.novax.prem.rural$si.flow.6)) 

AR.prem.urban.50_59 <- print(AR(df.novax.prem.urban$si.flow.6, init.novax.prem.urban$s.num.6)) 
ARv.prem.urban.50_59 <- print(ARv(df.vax.prem.urban$si.flow.6, init.novax.prem.urban$s.num.6)) 
ov.eff.prem.urban.50_59 <- print(overall.eff(df.vax.prem.urban$si.flow.6, df.novax.prem.urban$si.flow.6)) 

### age group 60+
AR.prem.rural.60 <- print(AR(df.novax.prem.rural$si.flow.7, init.novax.prem.rural$s.num.7)) 
ARv.prem.rural.60 <- print(ARv(df.vax.prem.rural$si.flow.7, init.novax.prem.rural$s.num.7)) 
ov.eff.prem.rural.60 <- print(overall.eff(df.vax.prem.rural$si.flow.7, df.novax.prem.rural$si.flow.7)) 

AR.prem.urban.60 <- print(AR(df.novax.prem.urban$si.flow.7, init.novax.prem.urban$s.num.7)) 
ARv.prem.urban.60 <- print(ARv(df.vax.prem.urban$si.flow.7, init.novax.prem.urban$s.num.7)) 
ov.eff.prem.urban.60 <- print(overall.eff(df.vax.prem.urban$si.flow.7, df.novax.prem.urban$si.flow.7)) 


# # NEXT STEPS
# 1. create VE dataframes with: empirical rural+urban and Prem rural+urban_day1_contacts
# Create a df for overall attack rates
OE.empirical.rur <- rbind(ov.eff.rur.0_9, ov.eff.rur.10_19, ov.eff.rur.20_29, ov.eff.rur.30_39, 
                          ov.eff.rur.40_49, ov.eff.rur.50_59, ov.eff.rur.60)
OE.empirical.urb <- rbind(ov.eff.urb.0_9, ov.eff.urb.10_19, ov.eff.urb.20_29, ov.eff.urb.30_39, 
                          ov.eff.urb.40_49, ov.eff.urb.50_59, ov.eff.urb.60)
OE.rural.prem <- rbind(ov.eff.prem.rural.0_9, ov.eff.prem.rural.10_19, ov.eff.prem.rural.20_29, 
                       ov.eff.prem.rural.30_39, ov.eff.prem.rural.40_49, ov.eff.prem.rural.50_59, 
                       ov.eff.prem.rural.60)
OE.urban.prem <- rbind(ov.eff.prem.urban.0_9, ov.eff.prem.urban.10_19, ov.eff.prem.urban.20_29, 
                       ov.eff.prem.urban.30_39, ov.eff.prem.urban.40_49, ov.eff.prem.urban.50_59, 
                       ov.eff.prem.urban.60)

OE_v3 <- cbind(OE.empirical.rur, OE.empirical.urb, OE.rural.prem, OE.urban.prem) %>%
  reshape2::melt(id.vvars="Xax") %>%
  mutate(site = case_when(Var2 == 1 ~ "ER",
                          Var2 == 2 ~ "EU",
                          Var2 == 3 ~ "SR",
                          Var2 == 4 ~ "SU")) %>%
  mutate(study_site = case_when(site == "ER" ~ "Rural",
                                site == "SR" ~ "Rural",
                                site == "EU" ~ "Urban",
                                site == "SU" ~ "Urban")) %>%
  mutate(age_group = case_when(Var1 == "ov.eff.rur.0_9" ~ "0-9y",
                               Var1 == "ov.eff.rur.10_19" ~ "10-19y",
                               Var1 == "ov.eff.rur.20_29" ~ "20-29y",
                               Var1 == "ov.eff.rur.30_39" ~ "30-39y",
                               Var1 == "ov.eff.rur.40_49" ~ "40-49y",
                               Var1 == "ov.eff.rur.50_59" ~ "50-59y",
                               Var1 == "ov.eff.rur.60" ~ "60+y"))

data4 <- OE_v3 %>%
  # filter(site != "Urban") %>%
  select(Var1, site, value) %>%
  mutate(site = factor(site,
                       levels = c("ER", "EU", "SR", "SU"))) %>%
  mutate(study_site = case_when(site == "ER" ~ "Rural",
                                site == "SR" ~ "Rural",
                                site == "EU" ~ "Urban",
                                site == "SU" ~ "Urban")) %>%
  pivot_wider(names_from = site, values_from = value) %>%
  mutate(change_prem_rural = SR - ER,
         change_prem_urban = SU - EU) %>%
  as.data.frame() %>%
  mutate(age_group = case_when(Var1 == "ov.eff.rur.0_9" ~ "0-9y",
                               Var1 == "ov.eff.rur.10_19" ~ "10-19y",
                               Var1 == "ov.eff.rur.20_29" ~ "20-29y",
                               Var1 == "ov.eff.rur.30_39" ~ "30-39y",
                               Var1 == "ov.eff.rur.40_49" ~ "40-49y",
                               Var1 == "ov.eff.rur.50_59" ~ "50-59y",
                               Var1 == "ov.eff.rur.60" ~ "60+y")) %>%
  select(-c(Var1))


# 2. create graph with Prem rural and urban VE compared to empirical by site.
# combined overall VE for Prem, rural and urban
oe_rural_v3 <- ggplot() + 
  geom_segment(data = data4,
               aes(x = SR, xend = ER, y = age_group, yend = age_group),
               col = 'grey40',
               linewidth = 1.25) +
  geom_point(
    data = OE_v3 %>% filter(study_site == "Rural"),
    aes(x = value, y = age_group, fill = site), 
    size = 3,
    shape = 21) +
  axis_text_theme2 +
  labs(
    x = 'Overall VE (%)',
    y = 'Age group',
    title = 'Rural') +
  scale_fill_manual(values = c("#d8b365", "#000000")) +
  scale_x_continuous(limits = c(0, 100)) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text.x = element_text(angle = 0),
    legend.position = c(0.15, 0.8),
    legend.direction = "vertical")
# oe_rural_v3

oe_urban_v3 <- ggplot() + 
  geom_segment(data = data4,
               aes(x = SU, xend = EU, y = age_group, yend = age_group),
               col = 'grey40',
               linewidth = 1.25) +
  geom_point(
    data = OE_v3 %>% filter(study_site == "Urban"),
    aes(x = value, y = age_group, fill = site), 
    size = 3,
    shape = 21) +
  axis_text_theme2 +
  labs(
    x = 'Overall VE (%)',
    y = 'Age group',
    title = 'Urban') +
  scale_fill_manual(values = c("#5ab4ac", "#000000")) +
  scale_x_continuous(limits = c(0, 100)) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text.x = element_text(angle = 0),
    legend.position = c(0.15, 0.8),
    legend.direction = "vertical")
# oe_urban_v3

oe_combined_v3 <- oe_rural_v3 | oe_urban_v3
oe_combined_v3
ggsave(oe_combined_v3, filename = "../../output/figs/oe_fig_site_2021data.pdf",
       height=4, width=8, dpi=300,
       bg="#FFFFFF") 

# 2. combine graphs.
combined_model_figure_v3 <- adjusted_matrix / oe_combined_v3
# combined_model_figure_v3
# 
combined_model_figure_v3 <- wrap_plots(adjusted_matrix, oe_combined_v3) + 
  plot_annotation(tag_levels = 'A') + 
  theme(plot.tag = element_text(size = 12)) +
  plot_layout(nrow=2, heights = c(600))

ggsave(combined_model_figure_v3, filename = "../../output/figs/fig2_matrix_model_2021data.pdf",
       height=6, width=8, dpi=300,
       bg="#FFFFFF") 

cat("End of model _v3 code.")