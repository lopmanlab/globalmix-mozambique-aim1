##########################################################################

# Code for producing weighted Mozambique contact matrix from prem et al
# August 30, 2023

##########################################################################


### Load libraries
library(tidyr)
library(dplyr)
library(ggplot2)


### read in social contact patterns
# default is by 5 year age groups
moz_prem <- read.csv("C:/Users/skim689/OneDrive - Emory University/Desktop/Mixing Studies/Global Mix/Mozambique/Contact Patterns Abstract/Pop Sizes/moz_prem_2017.csv")


### Generate data frame of participant age groups for orig prem matrix matched to age groups for target matrix
age_cat_part <- data.frame(part_age1=c("0_4","5_9","10_14","15_19","20_24","25_29","30_34","35_39",
                                       "40_44","45_49","50_54","55_59","60_64","65_69","70_74","75"),
                           part_age2 =c("[0,9)", "[0,9)", "[10,19)","[10,19)", "[20,29)", "[20,29)", "[30,39)", "[30,39)", 
                                        "[40,59)","[40,59)", "[40,59)", "[40,59)",  "60+", "60+", "60+", "60+"))


### Generate data frame of contact age groups for orig prem matrix matched to contact age groups for target matrix
age_cat_cont <- data.frame(cont_age1 = colnames(moz_prem),
                           cont_age2 = c("[0,9)", "[0,9)", "[10,19)","[10,19)", "[20,29)", "[20,29)", "[30,39)", "[30,39)", 
                                         "[40,59)","[40,59)", "[40,59)", "[40,59)",  "60+", "60+", "60+", "60+"))

moz_prem$part_age1 <- age_cat_part$part_age1


### pivot_longer for ease of data manipulation
moz_prem<- moz_prem%>%
  pivot_longer(cols = contact_0_4:contact_75, names_to = "cont_age",values_to = "avg_cont")

### Create a column for the age groups of the target matrix
moz_prem <-moz_prem %>%
  left_join(age_cat_part, by = c("part_age1"="part_age1")) %>%
  left_join(age_cat_cont, by = c("cont_age"="cont_age1"))


### population data for 5-year population distribution in Moz
moz_pop_5yr <- read.csv("C:/Users/skim689/OneDrive - Emory University/Desktop/Mixing Studies/Global Mix/Mozambique/Contact Patterns Abstract/Pop Sizes/agecat_5_total.csv")

### population data for 10-year population distribution in Moz
moz_pop_10yr <- read.csv("C:/Users/skim689/OneDrive - Emory University/Desktop/Mixing Studies/Global Mix/Mozambique/Contact Patterns Abstract/Pop Sizes/agecat_10_total_new.csv")


### "linearly" collapse the participant-contact cells based on population sizes
moz_prem10<- moz_prem %>%
  left_join(moz_pop_5yr, by = c("part_age1"="part_age1"))%>%  ## pop size of 5-year participant age band
  mutate(tot_pop_contacts = avg_cont*pop5yr)%>%   ## total contacts made between 0_4 & 0_4, 0_4&5_9 etc
  group_by(part_age2, cont_age2)%>%
  summarise(tot_pop_contacts = sum(tot_pop_contacts))%>% ##total contacts made between 0_9&0_9, 0_9&10_19 etc
  left_join(moz_pop_10yr, by = c("part_age2"="part_age2"))%>% # pop size of 10-year participant age band
  mutate(contacts = tot_pop_contacts/pop10yr)        ## average contacts made using 10-year age band

### plot
premmatrix <- ggplot(moz_prem10, aes(x = part_age2, y = cont_age2, fill = contacts)) + theme(legend.position = "bottom") + 
  scale_fill_gradient2(low = "white", high = "#273871", mid = "#7FABD3", midpoint = 3.4, limit = c(0,6))+
  xlab("Age of participant")+ylab("Age of contact")+
  geom_tile()+
  geom_text(aes(label=round(contacts, digits=2)), colour = "black", check_overlap = TRUE)+
  theme()
