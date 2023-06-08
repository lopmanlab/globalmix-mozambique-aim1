rm(list=ls())
library(here)
library(dplyr)
library(ggplot2)
library(plotly)

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

df_contact_d1 <- df_contact %>%
  dplyr::filter(study_day==1)

day1_num_contacts <- df_contact_d1 %>%
  dplyr::group_by(rec_id, study_site) %>%
  dplyr::summarize(num_contacts = n()) %>%
  dplyr::select(rec_id, num_contacts)

df_contact_d1 <- left_join(df_contact_d1, day1_num_contacts, by="rec_id" )

df_contact_d2 <- df_contact %>%
  dplyr::filter(study_day==2)

day2_num_contacts <- df_contact_d2 %>%
  dplyr::group_by(rec_id, study_site) %>%
  dplyr::summarize(num_contacts = n()) %>%
  dplyr::select(rec_id, num_contacts)

df_contact_d2 <- left_join(df_contact_d2, day2_num_contacts, by="rec_id" )

both_num_contacts <- df_contact %>%
  dplyr::group_by(rec_id, study_day, study_site) %>%
  dplyr::summarize(num_contacts = n()) %>%
  dplyr::select(rec_id, study_day, num_contacts)

df_contact <- left_join(df_contact, both_num_contacts, by=c("rec_id", "study_day") )

# rural contacts
df_contact_rural <- df_contact %>%
  dplyr::filter(study_site == "Rural")

## specify participant characteristic stratification for cross tabs/analysis
variables <- data.frame(var=c("participant_sex","participant_age", "mask_uselb", 
                              "hh_membership","occupation", "weekday", "enrolled_school",
                              "touch_contact", "where_contact", "cnt_home", "cnt_school",
                              "cnt_work", "cnt_otherplace", "frequency_contact",
                              "known_contact", "contact_mask2", "ari_symptom", "age_symptom"),
                        # "hh_member_relationship",
                        name=c("Sex", "Age", "Do you wear a mask?",
                               "Household membership", "Occupation", "Weekday/Weekend",
                               "Enrolled in school", "Did you touch?", "Contact location",
                               "Contact at home", "Contact at work", 
                               "Contact at school", "Contact in other locations",
                               "Frequency of contact", "Do you know the contact?", 
                               "Contact wearing mask", "ARI symptoms", "AGE symptoms")) %>%
                        #  "Relationship to household member",
  
  mutate(var = as.character(var),
         name = as.character(name))

list <- list(0)

for (i in 1:nrow(variables)){
  x <- df_contact_rural[,variables$var[[i]]] 
  # include another variable n as the number of participants per strata
  variables$var[[i]]
  
# Number and proportion of contacts in each strata
  
  t0 <- as.data.frame(cbind(table(x), # Total 
                            round(prop.table(table(x))*100, digits=0) # Proportion
                            )) # number of participants
 
  colnames(t0)[1:2] <- c("Total","Col") 
  Tot <- rep("",5)
  
# Median contacts for day 1
  t1 <- df_contact_d1 %>%
    dplyr::filter(study_site == "Rural") %>%
    # since this is total contacts per person, keep only 1 distinct record
    distinct(rec_id, .keep_all = TRUE) %>% 
    dplyr::group_by(.dots = variables$var[[i]]) %>% 
    # na.omit() %>%
    do(data.frame(t(quantile(.$num_contacts, na.rm=TRUE, probs=c(0.25,0.5,0.75))))) # Median and IQR      
  t1$med_contact <- as.character(paste(t1$X50.," (",t1$X25.,"-",t1$X75.,")",sep="")) # Formatting for export
  
  # Median contacts for day 2
  t2 <- df_contact_d2 %>%
    dplyr::filter(study_site == "Rural") %>%
    # since this is total contacts per person, keep only 1 distinct record
    distinct(rec_id, .keep_all = TRUE) %>% 
    dplyr::group_by(.dots = variables$var[[i]]) %>%
    # na.omit() %>%
    do(data.frame(t(quantile(.$num_contacts, na.rm=TRUE, probs=c(0.25,0.5,0.75))))) # Median and IQR      
  t2$med_contact <- as.character(paste(t2$X50.," (",t2$X25.,"-",t2$X75.,")",sep="")) # Formatting for export
  
  # Bind columns together and select relevant columns
  t0<-qpcR:::cbind.na(t0, t1$med_contact, t2$med_contact) 
  
  # convert to numerical and round off?
  
  t0<- t0[,c(1,2,3,4)]
  
  # rename total column
  t0$Total <- paste(t0$Total," (","", t0$Col,")",sep="")
  t0 <- t0[,c(1,3,4)]
  t0[,1]<-as.character(t0[,1])
  t0[,2]<-as.character(t0[,2])

  t0<-rbind(Tot, t0)
  
  rownames(t0)[1]<-variables$name[[i]]
  list[[i]] <- t0
}

# format table
res_restruct<- function(res){
  res1 <- lapply(res, as.data.frame)
  res1 <- do.call(rbind, res1)
  return(res1)
}

table3 <- res_restruct(list)
colnames(table3) <- c("Total (%)", "Day 1", "Day 2")

table3 <- knitr::kable(table3, digits = 0, align = "r") %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed"))

table3


## Urban Contact summary----------------------------------------------------
df_contact_urban <- df_contact %>%
  dplyr::filter(study_site == "Urban")

list <- list(0)

for (i in 1:nrow(variables)){
  x <- df_contact_urban[,variables$var[[i]]] 
  # include another variable n as the number of participants per strata
  variables$var[[i]]
  
# Number and proportion of contacts in each strata
  
  t0 <- as.data.frame(cbind(table(x), # Total 
                            round(prop.table(table(x))*100, digits=0) # Proportion
                            )) # number of participants
 
  colnames(t0)[1:2] <- c("Total","Col") 
  Tot <- rep("",5)
  
# Median contacts for day 1
  t1 <- df_contact_d1 %>%
    dplyr::filter(study_site == "Urban") %>%
    # since this is total contacts per person, keep only 1 distinct record
    distinct(rec_id, .keep_all = TRUE) %>% 
    dplyr::group_by(.dots = variables$var[[i]]) %>% 
    # na.omit() %>%
    do(data.frame(t(quantile(.$num_contacts, na.rm=TRUE, probs=c(0.25,0.5,0.75))))) # Median and IQR      
  t1$med_contact <- as.character(paste(t1$X50.," (",t1$X25.,"-",t1$X75.,")",sep="")) # Formatting for export
  
  # Median contacts for day 2
  t2 <- df_contact_d2 %>%
    dplyr::filter(study_site == "Urban") %>%
    # since this is total contacts per person, keep only 1 distinct record
    distinct(rec_id, .keep_all = TRUE) %>% 
    dplyr::group_by(.dots = variables$var[[i]]) %>%
    # na.omit() %>%
    do(data.frame(t(quantile(.$num_contacts, na.rm=TRUE, probs=c(0.25,0.5,0.75))))) # Median and IQR      
  t2$med_contact <- as.character(paste(t2$X50.," (",t2$X25.,"-",t2$X75.,")",sep="")) # Formatting for export
  
  # Bind columns together and select relevant columns
  t0<-qpcR:::cbind.na(t0, t1$med_contact, t2$med_contact) 
  
  # convert to numerical and round off?
  
  t0<- t0[,c(1,2,3,4)]
  
  # rename total column
  t0$Total <- paste(t0$Total," (","", t0$Col,")",sep="")
  t0 <- t0[,c(1,3,4)]
  t0[,1]<-as.character(t0[,1])
  t0[,2]<-as.character(t0[,2])

  t0<-rbind(Tot, t0)
  
  rownames(t0)[1]<-variables$name[[i]]
  list[[i]] <- t0
}

# format table
res_restruct<- function(res){
  res1 <- lapply(res, as.data.frame)
  res1 <- do.call(rbind, res1)
  return(res1)
}

table3 <- res_restruct(list)
colnames(table3) <- c("Total (%)", "Day 1", "Day 2")

table3 <- knitr::kable(table3, digits = 0, align = "r") %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed"))

table3


## Day 1 contacts Histograms ----------------------------------------------

contacts_age <- df_contact_d1 %>%
  dplyr::group_by(rec_id, study_site, participant_age) %>%   
  # group by id and count number of df_contact_d1
  dplyr::summarize(num_contacts = n())


# contacts by site
contacts_site_d1 <- df_contact_d1 %>%
  dplyr::group_by(rec_id, study_site) %>%
  dplyr::summarize(num_contacts = n())

median_contacts_site_d1 <- contacts_site_d1 %>%
  dplyr::group_by(study_site) %>%
  dplyr::summarize(q25 = quantile(num_contacts, probs = 0.25),
                   q50 = quantile(num_contacts, probs = 0.5),
                   q75 = quantile(num_contacts, probs = 0.75))

mean_contacts_site_d1 <- contacts_site_d1 %>%
  dplyr::group_by(study_site) %>%
  dplyr::summarize(mean = round(mean(num_contacts),1))

ci_contacts_rural_d1 <- contacts_site_d1 %>%
  dplyr::filter(study_site == "Rural")
ci_contacts_rural_d1 <- lm(num_contacts ~ 1, ci_contacts_rural_d1)
ci_contacts_rural_d1 <- as.data.frame(round(confint(ci_contacts_rural_d1),1))

ci_contacts_urban_d1 <- contacts_site_d1 %>%
  dplyr::filter(study_site == "Urban")
ci_contacts_urban_d1 <- lm(num_contacts ~ 1, ci_contacts_urban_d1)
ci_contacts_urban_d1 <- as.data.frame(round(confint(ci_contacts_urban_d1),1))

# title
title_rural_d1 <- list(
  text = "Rural",
  size=14,
  # font = f,
  xref = "paper", yref = "paper",
  yanchor = "bottom", xanchor = "center",
  align = "center",
  x = 0.5, y = 1,
  showarrow = FALSE
)

title_urban_d1 <- list(
  text = "Urban",
  size=14,
  # font = f,
  xref = "paper", yref = "paper",
  yanchor = "bottom", xanchor = "center",
  align = "center",
  x = 0.5, y = 1,
  showarrow = FALSE
)

hist_rural_d1 <- contacts_site_d1 %>%
  dplyr::filter(study_site == "Rural") %>%
  plotly::plot_ly(x=~num_contacts, colors="#a6611a") %>%
  plotly::add_lines(y = range(0:100),
            x = mean_contacts_site_d1$mean[1],
            line = list(color = "black"),
            showlegend = FALSE) %>%
  add_lines(y = range(0:100),
            x = median_contacts_site_d1$q50[1],
            line = list(color = "grey"),
            showlegend = FALSE) %>%
  plotly::add_annotations(x = mean_contacts_site_d1$mean[1], 
                  y = 90,
                  text = paste(mean_contacts_site_d1$mean[1], " (95% CI ",
                               ci_contacts_rural_d1$`2.5 %`,"-",
                               ci_contacts_rural_d1$`97.5 %`,")", sep=""), 
                  showarrow = F, xanchor = "left") %>%
  add_annotations(x = median_contacts_site_d1$q50[1], 
                  y = 90,
                  text = paste(median_contacts_site_d1$q50[1], " (95% CI ",
                               median_contacts_site_d1$q25[1],"-",
                               median_contacts_site_d1$q75[1],")", sep=""), 
                  showarrow = F, xanchor = "right") %>%
  plotly::layout(annotations = title_rural_d1,
         xaxis = list(title = "Num of contacts")) %>%
  plotly::add_histogram(name="Rural")
# hist_rural

hist_urban_d1 <- contacts_site_d1 %>%
  dplyr::filter(study_site == "Urban") %>%
  plotly::plot_ly(x=~num_contacts, colors="#018571") %>%
  add_lines(y = range(0:100),
            x = mean_contacts_site_d1$mean[2],
            line = list(color = "black"),
            showlegend = FALSE) %>%
  add_lines(y = range(0:100),
            x = median_contacts_site_d1$q50[2],
            line = list(color = "grey"),
            showlegend = FALSE) %>%
  add_annotations(x = mean_contacts_site_d1$mean[2], 
                  y = 90,
                  text = paste(mean_contacts_site_d1$mean[2], " (95% CI ",
                               ci_contacts_urban_d1$`2.5 %`,"-",
                               ci_contacts_urban_d1$`97.5 %`,")", sep=""), 
                  showarrow = F, xanchor = "left") %>%
  add_annotations(x = median_contacts_site_d1$q50[2], 
                  y = 90,
                  text = paste(median_contacts_site_d1$q50[2], " (95% CI ",
                               median_contacts_site_d1$q25[2],"-",
                               median_contacts_site_d1$q75[2],")", sep=""), 
                  showarrow = F, xanchor = "right") %>%
  layout(annotations = title_urban_d1,
         xaxis = list(title = "Num of contacts")) %>%
  plotly::add_histogram(name="Urban")

fig1_hist_d1 <- subplot(hist_rural_d1, hist_urban_d1, 
                        nrows=2, shareX = TRUE) %>% 
  layout(title = 'Day 1 Contact Distributions')

fig1_hist_d1


## Day 2 contacts Histograms ----------------------------------------------

contacts_age <- df_contact_d2 %>%
  dplyr::group_by(rec_id, study_site, participant_age) %>%   
  # group by id and count number of df_contact_d2
  dplyr::summarize(num_contacts = n())


# contacts by site
contacts_site_d2 <- df_contact_d2 %>%
  dplyr::group_by(rec_id, study_site) %>%
  dplyr::summarize(num_contacts = n())

median_contacts_site_d2 <- contacts_site_d2 %>%
  dplyr::group_by(study_site) %>%
  dplyr::summarize(q25 = quantile(num_contacts, probs = 0.25),
                   q50 = quantile(num_contacts, probs = 0.5),
                   q75 = quantile(num_contacts, probs = 0.75))

mean_contacts_site_d2 <- contacts_site_d2 %>%
  dplyr::group_by(study_site) %>%
  dplyr::summarize(mean = round(mean(num_contacts),1))

ci_contacts_rural_d2 <- contacts_site_d2 %>%
  dplyr::filter(study_site == "Rural")
ci_contacts_rural_d2 <- lm(num_contacts ~ 1, ci_contacts_rural_d2)
ci_contacts_rural_d2 <- as.data.frame(round(confint(ci_contacts_rural_d2),1))

ci_contacts_urban_d2 <- contacts_site_d2 %>%
  dplyr::filter(study_site == "Urban")
ci_contacts_urban_d2 <- lm(num_contacts ~ 1, ci_contacts_urban_d2)
ci_contacts_urban_d2 <- as.data.frame(round(confint(ci_contacts_urban_d2),1))

# title
title_rural_d2 <- list(
  text = "Rural",
  size=14,
  # font = f,
  xref = "paper", yref = "paper",
  yanchor = "bottom", xanchor = "center",
  align = "center",
  x = 0.5, y = 1,
  showarrow = FALSE
)

title_urban_d2 <- list(
  text = "Urban",
  size=14,
  # font = f,
  xref = "paper", yref = "paper",
  yanchor = "bottom", xanchor = "center",
  align = "center",
  x = 0.5, y = 1,
  showarrow = FALSE
)

hist_rural_d2 <- contacts_site_d2 %>%
  dplyr::filter(study_site == "Rural") %>%
  plotly::plot_ly(x=~num_contacts, colors="#a6611a") %>%
  plotly::add_lines(y = range(0:100),
            x = mean_contacts_site_d2$mean[1],
            line = list(color = "black"),
            showlegend = FALSE) %>%
  add_lines(y = range(0:100),
            x = median_contacts_site_d2$q50[1],
            line = list(color = "grey"),
            showlegend = FALSE) %>%
  plotly::add_annotations(x = mean_contacts_site_d2$mean[1], 
                  y = 90,
                  text = paste(mean_contacts_site_d2$mean[1], " (95% CI ",
                               ci_contacts_rural_d2$`2.5 %`,"-",
                               ci_contacts_rural_d2$`97.5 %`,")", sep=""), 
                  showarrow = F, xanchor = "left") %>%
  add_annotations(x = median_contacts_site_d2$q50[1], 
                  y = 90,
                  text = paste(median_contacts_site_d2$q50[1], " (95% CI ",
                               median_contacts_site_d2$q25[1],"-",
                               median_contacts_site_d2$q75[1],")", sep=""), 
                  showarrow = F, xanchor = "right") %>%
  plotly::layout(annotations = title_rural_d2,
         xaxis = list(title = "Num of contacts")) %>%
  plotly::add_histogram(name="Rural")
# hist_rural

hist_urban_d2 <- contacts_site_d2 %>%
  dplyr::filter(study_site == "Urban") %>%
  plotly::plot_ly(x=~num_contacts, colors="#018571") %>%
  add_lines(y = range(0:100),
            x = mean_contacts_site_d2$mean[2],
            line = list(color = "black"),
            showlegend = FALSE) %>%
  add_lines(y = range(0:100),
            x = median_contacts_site_d2$q50[2],
            line = list(color = "grey"),
            showlegend = FALSE) %>%
  add_annotations(x = mean_contacts_site_d2$mean[2], 
                  y = 90,
                  text = paste(mean_contacts_site_d2$mean[2], " (95% CI ",
                               ci_contacts_urban_d2$`2.5 %`,"-",
                               ci_contacts_urban_d2$`97.5 %`,")", sep=""), 
                  showarrow = F, xanchor = "left") %>%
  add_annotations(x = median_contacts_site_d2$q50[2], 
                  y = 90,
                  text = paste(median_contacts_site_d2$q50[2], " (95% CI ",
                               median_contacts_site_d2$q25[2],"-",
                               median_contacts_site_d2$q75[2],")", sep=""), 
                  showarrow = F, xanchor = "right") %>%
  layout(annotations = title_urban_d2,
         xaxis = list(title = "Num of contacts")) %>%
  plotly::add_histogram(name="Urban")

fig1_hist_d2 <- subplot(hist_rural_d2, hist_urban_d2, 
                        nrows=2, shareX = TRUE) %>% 
  layout(title = 'Day 2 Contact Distributions')

fig1_hist_d2


## Total Contacts Histograms -----------------------------------------------

# contacts by site
contacts_site <- df_contact %>%
  dplyr::group_by(rec_id, study_site, study_day, participant_age, participant_sex, age) %>%
  dplyr::summarize(num_contacts = n())
  
median_contacts_site <- contacts_site %>%
  dplyr::group_by(study_site) %>%
  dplyr::summarize(q25 = quantile(num_contacts, probs = 0.25),
                   q50 = quantile(num_contacts, probs = 0.5),
                   q75 = quantile(num_contacts, probs = 0.75))

median_contacts <- contacts_site %>%
  ungroup() %>%
  dplyr::summarize(q25 = quantile(num_contacts, probs = 0.25),
                   q50 = quantile(num_contacts, probs = 0.5),
                   q75 = quantile(num_contacts, probs = 0.75))

mean_contacts_site <- contacts_site %>%
  dplyr::group_by(study_site) %>%
  dplyr::summarize(mean = round(mean(num_contacts),1))

mean_contacts <- contacts_site %>%
  ungroup() %>%
  dplyr::summarize(mean = round(mean(num_contacts),1))

ci_contacts_rural <- contacts_site %>%
  dplyr::filter(study_site == "Rural")
ci_contacts_rural <- lm(num_contacts ~ 1, ci_contacts_rural)
ci_contacts_rural <- as.data.frame(round(confint(ci_contacts_rural),1))

ci_contacts_urban <- contacts_site %>%
  dplyr::filter(study_site == "Urban")
ci_contacts_urban <- lm(num_contacts ~ 1, ci_contacts_urban)
ci_contacts_urban <- as.data.frame(round(confint(ci_contacts_urban),1))

ci_contacts <- contacts_site
ci_contacts <- lm(num_contacts ~ 1, ci_contacts)
ci_contacts <- as.data.frame(round(confint(ci_contacts),1))

# title
title_rural <- list(
  text = "Rural",
  size=14,
  # font = f,
  xref = "paper", yref = "paper",
  yanchor = "bottom", xanchor = "center",
  align = "center",
  x = 0.5, y = 1,
  showarrow = FALSE
)

title_urban <- list(
  text = "Urban",
  size=14,
  # font = f,
  xref = "paper", yref = "paper",
  yanchor = "bottom", xanchor = "center",
  align = "center",
  x = 0.5, y = 1,
  showarrow = FALSE
)

title_both <- list(
  text = "Both Sites",
  size=14,
  # font = f,
  xref = "paper", yref = "paper",
  yanchor = "bottom", xanchor = "center",
  align = "center",
  x = 0.5, y = 1,
  showarrow = FALSE
)

library(wesanderson)
cols <- wes_palette("Darjeeling1") #green is 2, blue is 5, yellow is 3

hist_rural <- contacts_site %>%
  dplyr::filter(study_site == "Rural") %>%
  plotly::plot_ly(x=~num_contacts) %>%
  plotly::add_lines(y = range(0:150),
            x = mean_contacts_site$mean[1],
            line = list(color = "black"),
            showlegend = FALSE) %>%
  add_lines(y = range(0:150),
            x = median_contacts_site$q50[1],
            line = list(color = "grey"),
            showlegend = FALSE) %>%
  plotly::add_annotations(x = mean_contacts_site$mean[1], 
                  y = 90,
                  text = paste(mean_contacts_site$mean[1], " (95% CI ",
                               ci_contacts_rural$`2.5 %`,"-",
                               ci_contacts_rural$`97.5 %`,")", sep=""), 
                  showarrow = F, xanchor = "left") %>%
  add_annotations(x = median_contacts_site$q50[1], 
                  y = 90,
                  text = paste(median_contacts_site$q50[1], " (95% CI ",
                               median_contacts_site$q25[1],"-",
                               median_contacts_site$q75[1],")", sep=""), 
                  showarrow = F, xanchor = "right") %>%
  plotly::layout(annotations = title_rural,
         xaxis = list(title = "Num of contacts")) %>%
  plotly::add_histogram(name="Rural")
# hist_rural

hist_urban <- contacts_site %>%
  dplyr::filter(study_site == "Urban") %>%
  plotly::plot_ly(x=~num_contacts) %>%
  add_lines(y = range(0:175),
            x = mean_contacts_site$mean[2],
            line = list(color = "black"),
            showlegend = FALSE) %>%
  add_lines(y = range(0:175),
            x = median_contacts_site$q50[2],
            line = list(color = "grey"),
            showlegend = FALSE) %>%
  add_annotations(x = mean_contacts_site$mean[2], 
                  y = 90,
                  text = paste(mean_contacts_site$mean[2], " (95% CI ",
                               ci_contacts_urban$`2.5 %`,"-",
                               ci_contacts_urban$`97.5 %`,")", sep=""), 
                  showarrow = F, xanchor = "left") %>%
  add_annotations(x = median_contacts_site$q50[2], 
                  y = 90,
                  text = paste(median_contacts_site$q50[2], " (95% CI ",
                               median_contacts_site$q25[2],"-",
                               median_contacts_site$q75[2],")", sep=""), 
                  showarrow = F, xanchor = "right") %>%
  layout(annotations = title_urban,
         xaxis = list(title = "Num of contacts")) %>%
  plotly::add_histogram(name="Urban")

hist_both <- contacts_site %>%
  plotly::plot_ly(x=~num_contacts) %>%
  add_lines(y = range(0:300),
            x = mean_contacts$mean,
            line = list(color = "black"),
            showlegend = FALSE) %>%
  add_lines(y = range(0:300),
            x = median_contacts$q50[2],
            line = list(color = "grey"),
            showlegend = FALSE) %>%
  add_annotations(x = mean_contacts$mean, 
                  y = 90,
                  text = paste(mean_contacts$mean, " (95% CI ",
                               ci_contacts$`2.5 %`,"-",
                               ci_contacts$`97.5 %`,")", sep=""), 
                  showarrow = F, xanchor = "left") %>%
  add_annotations(x = median_contacts$q50, 
                  y = 90,
                  text = paste(median_contacts$q50, " (95% CI ",
                               median_contacts$q25,"-",
                               median_contacts$q75,")", sep=""), 
                  showarrow = F, xanchor = "right") %>%
  layout(annotations = title_both,
         xaxis = list(title = "Num of contacts")) %>%
  plotly::add_histogram(name="Both Sites")

fig1_hist <- subplot(hist_rural, hist_urban, hist_both,
                        nrows=3, shareX = TRUE) %>% 
  layout(title = 'Contact Distributions - Both Days')

fig1_hist


## Fitting total contacts ---------------------------------------------------
fitn <- fitdistrplus::fitdist(contacts_site$num_contacts, "norm")
summary(fitn)
plot(fitn)

fitp <- fitdistrplus::fitdist(contacts_site$num_contacts, "pois")
summary(fitp)
plot(fitp)

fitnb <- fitdistrplus::fitdist(contacts_site$num_contacts, "nbinom")
summary(fitnb)
plot(fitnb)

ggplot()+
  geom_histogram(data=contacts_site, aes(x = num_contacts,y = ..density..), 
                 color = cols[2], 
                 fill = cols[2])+
  geom_vline(aes(xintercept = mean_contacts$mean))+
  geom_vline(aes(xintercept = median_contacts$q50), col = "grey")+
  stat_function(fun = dnorm, args = list(mean = fitn$estimate[1], sd = fitn$estimate[2]), aes(lty="Normal"))+
  stat_function(fun = dpois, args = list(lambda = fitp$estimate[1]), aes(lty="Poisson"))+
  stat_function(fun = dnbinom, args = list(size = fitnb$estimate[1], mu = fitnb$estimate[2], aes(lty = "Neg Binomial")))+
  theme_minimal()+
  scale_colour_manual(name = "Fitted Distributions", values = c(1, 2, 3))+
  labs(x = "Total Contacts", y = "Density", 
       lty = "Fitted Distributions", 
       title = "Total Contacts at Both Sites with Distributions")
  
#URBAN
urban <- contacts_site %>% filter(study_site == "Urban")

fitn <- fitdistrplus::fitdist(urban$num_contacts, "norm")
summary(fitn)
plot(fitn)

fitp <- fitdistrplus::fitdist(urban$num_contacts, "pois")
summary(fitp)
plot(fitp)

fitnb <- fitdistrplus::fitdist(urban$num_contacts, "nbinom")
summary(fitnb)
plot(fitnb)

ggplot()+
  geom_histogram(data=urban, aes(x = num_contacts,y = ..density..), 
                 color = cols[5], 
                 fill = cols[5])+
  geom_vline(aes(xintercept = mean_contacts_site$mean[2]))+
  geom_vline(aes(xintercept = median_contacts_site$q50[2]), col = "grey")+
  stat_function(fun = dnorm, args = list(mean = fitn$estimate[1], sd = fitn$estimate[2]), aes(lty="Normal"))+
  stat_function(fun = dpois, args = list(lambda = fitp$estimate[1]), aes(lty="Poisson"))+
  stat_function(fun = dnbinom, args = list(size = fitnb$estimate[1], mu = fitnb$estimate[2], aes(lty = "Neg Binomial")))+
  theme_minimal()+
  scale_colour_manual(name = "Fitted Distributions", values = c(1, 2, 3))+
  labs(x = "Total Contacts", y = "Density", 
       lty = "Fitted Distributions", 
       title = "Total Contacts at Urban Site with Distributions")
  
#RURAL
rural <- contacts_site %>% filter(study_site == "Rural")

fitn <- fitdistrplus::fitdist(rural$num_contacts, "norm")
summary(fitn)
plot(fitn)

fitp <- fitdistrplus::fitdist(rural$num_contacts, "pois")
summary(fitp)
plot(fitp)

fitnb <- fitdistrplus::fitdist(rural$num_contacts, "nbinom")
summary(fitnb)
plot(fitnb)

ggplot()+
  geom_histogram(data=rural, aes(x = num_contacts,y = ..density..), 
                 color = cols[3], 
                 fill = cols[3])+
  geom_vline(aes(xintercept = mean_contacts$mean[1]))+
  geom_vline(aes(xintercept = median_contacts$q50[1]), col = "grey")+
  stat_function(fun = dnorm, args = list(mean = fitn$estimate[1], sd = fitn$estimate[2]), aes(lty="Normal"))+
  stat_function(fun = dpois, args = list(lambda = fitp$estimate[1]), aes(lty="Poisson"))+
  stat_function(fun = dnbinom, args = list(size = fitnb$estimate[1], mu = fitnb$estimate[2], aes(lty = "Neg Binomial")))+
  theme_minimal()+
  scale_colour_manual(name = "Fitted Distributions", values = c(1, 2, 3))+
  labs(x = "Total Contacts", y = "Density", 
       lty = "Fitted Distributions", 
       title = "Total Contacts at Rural Site with Distributions")
  


## Total Contacts Mixed models --------------------------------------------
df_contact <- df_contact %>% 
  group_by(study_site) %>%
  mutate(N = sum(num_contacts))
df_contact$s_age <- scale(as.numeric(df_contact$age))

library(lme4)
require(MASS)
library(lmerTest)

# negbin_model <- glmer.nb(num_contacts ~ age + participant_sex +
#                          study_site + (1 | rec_id),
#                          offset = log(N),
#                          verbose = TRUE,
#                          data = df_contact %>% distinct())
# summary(negbin_model)

# nb_int_model <- glmer.nb(num_contacts ~ age + participant_sex +
#                          study_site + participant_sex*study_site +
#                          age*study_site + age*participant_sex+
#                          (1 | rec_id),
#                          offset = log(N),
#                          verbose = TRUE,
#                          data = df_contact %>% distinct())
# summary(nb_int_model)

# poisson_model <- glmer(num_contacts ~ age + participant_sex + 
#                          study_site + (1 | rec_id),
#                       offset = log(N),
#                       data = df_contact %>% distinct(),
#                       family = poisson(link = "log"))
# 
# summary(poisson_model)

# poi_int_model <- glmer(num_contacts ~ age + participant_sex +
#                          study_site + participant_sex*study_site +
#                          age*study_site + age*participant_sex +
#                          (1 | rec_id),
#                       offset = log(N),
#                       data = df_contact %>% distinct(),
#                       family = poisson(link = "log"))
# summary(poi_int_model)

# linear_model <- lmer(num_contacts ~ age + participant_sex +
#                          study_site + (1 | rec_id),
#                       offset = log(N),
#                       data = df_contact %>% distinct())
# 
# summary(linear_model)

# lin_int_model <- lmer(num_contacts ~ age + participant_sex +
#                         study_site + participant_sex*study_site +
#                         age*study_site + age*participant_sex +
#                         (1 | rec_id),
#                       offset = log(N),
#                       data = df_contact %>% distinct())
# 
# summary(lin_int_model)

## Average Daily Unique Contacts Histograms ---------------------------------------

contact_unique <- contacts_site %>%
  dplyr::group_by(rec_id, study_site, participant_age, participant_sex, age) %>%
  summarise(avg_unique_contacts = round(mean(num_contacts)))

median_unique_site <- contact_unique %>%
  dplyr::group_by(study_site) %>%
  dplyr::summarize(q25 = quantile(avg_unique_contacts, probs = 0.25),
                   q50 = quantile(avg_unique_contacts, probs = 0.5),
                   q75 = quantile(avg_unique_contacts, probs = 0.75))

median_unique <- contact_unique %>%
  ungroup() %>%
  dplyr::summarize(q25 = quantile(avg_unique_contacts, probs = 0.25),
                   q50 = quantile(avg_unique_contacts, probs = 0.5),
                   q75 = quantile(avg_unique_contacts, probs = 0.75))

mean_unique_site <- contact_unique %>%
  dplyr::group_by(study_site) %>%
  dplyr::summarize(mean = round(mean(avg_unique_contacts),1))

mean_unique <- contact_unique %>%
  ungroup() %>%
  dplyr::summarize(mean = round(mean(avg_unique_contacts),1))

ci_contacts_rural <- contact_unique %>%
  dplyr::filter(study_site == "Rural")
ci_contacts_rural <- lm(avg_unique_contacts ~ 1, ci_contacts_rural)
ci_contacts_rural <- as.data.frame(round(confint(ci_contacts_rural),1))

ci_contacts_urban <- contact_unique %>%
  dplyr::filter(study_site == "Urban")
ci_contacts_urban <- lm(avg_unique_contacts ~ 1, ci_contacts_urban)
ci_contacts_urban <- as.data.frame(round(confint(ci_contacts_urban),1))

ci_contacts <- contact_unique
ci_contacts <- lm(avg_unique_contacts ~ 1, contact_unique)
ci_contacts <- as.data.frame(round(confint(ci_contacts),1))

fitn <- fitdistrplus::fitdist(contact_unique$avg_unique_contacts, "norm")
summary(fitn)
plot(fitn)

fitp <- fitdistrplus::fitdist(contact_unique$avg_unique_contacts, "pois")
summary(fitp)
plot(fitp)

fitnb <- fitdistrplus::fitdist(contact_unique$avg_unique_contacts, "nbinom")
summary(fitnb)
plot(fitnb)

ggplot()+
  geom_histogram(data=contact_unique, aes(x = avg_unique_contacts,y = ..density..), 
                 color = cols[2], 
                 fill = cols[2])+
  geom_vline(aes(xintercept = mean_unique$mean))+
  geom_vline(aes(xintercept = median_unique$q50), col = "grey")+
  stat_function(fun = dnorm, args = list(mean = fitn$estimate[1], sd = fitn$estimate[2]), aes(lty="Normal"))+
  stat_function(fun = dpois, args = list(lambda = fitp$estimate[1]), aes(lty="Poisson"))+
  stat_function(fun = dnbinom, args = list(size = fitnb$estimate[1], mu = fitnb$estimate[2], aes(lty = "Neg Binomial")))+
  theme_minimal()+
  scale_colour_manual(name = "Fitted Distributions", values = c(1, 2, 3))+
  labs(x = "Average Daily Unique Contacts", y = "Density", 
       lty = "Fitted Distributions", 
       title = "Average Daily Unique Contacts at Both Sites with Distributions")

#URBAN
urban <- contact_unique %>% filter(study_site == "Urban")

fitn <- fitdistrplus::fitdist(urban$avg_unique_contacts, "norm")
summary(fitn)
plot(fitn)

fitp <- fitdistrplus::fitdist(urban$avg_unique_contacts, "pois")
summary(fitp)
plot(fitp)

fitnb <- fitdistrplus::fitdist(urban$avg_unique_contacts, "nbinom")
summary(fitnb)
plot(fitnb)

ggplot()+
  geom_histogram(data=urban, aes(x = avg_unique_contacts,y = ..density..), 
                 color = cols[5], 
                 fill = cols[5])+
  geom_vline(aes(xintercept = mean_unique_site$mean[2]))+
  geom_vline(aes(xintercept = median_unique_site$q50[2]), col = "grey")+
  stat_function(fun = dnorm, args = list(mean = fitn$estimate[1], sd = fitn$estimate[2]), aes(lty="Normal"))+
  stat_function(fun = dpois, args = list(lambda = fitp$estimate[1]), aes(lty="Poisson"))+
  stat_function(fun = dnbinom, args = list(size = fitnb$estimate[1], mu = fitnb$estimate[2], aes(lty = "Neg Binomial")))+
  theme_minimal()+
  scale_colour_manual(name = "Fitted Distributions", values = c(1, 2, 3))+
  labs(x = "Average Daily Unique Contacts", y = "Density", 
       lty = "Fitted Distributions", 
       title = "Average Daily Unique Contacts at Urban Site with Distributions")

#RURAL
rural <- contact_unique %>% filter(study_site == "Rural")

fitn <- fitdistrplus::fitdist(rural$avg_unique_contacts, "norm")
summary(fitn)
plot(fitn)

fitp <- fitdistrplus::fitdist(rural$avg_unique_contacts, "pois")
summary(fitp)
plot(fitp)

fitnb <- fitdistrplus::fitdist(rural$avg_unique_contacts, "nbinom")
summary(fitnb)
plot(fitnb)

ggplot()+
  geom_histogram(data=rural, aes(x = avg_unique_contacts,y = ..density..), 
                 color = cols[3], 
                 fill = cols[3])+
  geom_vline(aes(xintercept = mean_unique$mean[1]))+
  geom_vline(aes(xintercept = median_unique$q50[1]), col = "grey")+
  stat_function(fun = dnorm, args = list(mean = fitn$estimate[1], sd = fitn$estimate[2]), aes(lty="Normal"))+
  stat_function(fun = dpois, args = list(lambda = fitp$estimate[1]), aes(lty="Poisson"))+
  stat_function(fun = dnbinom, args = list(size = fitnb$estimate[1], mu = fitnb$estimate[2], aes(lty = "Neg Binomial")))+
  theme_minimal()+
  scale_colour_manual(name = "Fitted Distributions", values = c(1, 2, 3))+
  labs(x = "Average Daily Unique Contacts", y = "Density", 
       lty = "Fitted Distributions", 
       title = "Average Daily Unique Contacts at Rural Site with Distributions")

## Average Daily Contacts Histograms ---------------------------------------
c <- df_contact %>%
  dplyr::group_by(rec_id, fromdayone, study_site, participant_age, 
                  participant_sex, age) %>%
  dplyr::summarize(contacts = n())

contacts_daily <- tidyr::pivot_wider(c, names_from = fromdayone, values_from=contacts)
contacts_daily$`Both Days`[which(is.na(contacts_daily$`Both Days`))] <- 0
contacts_daily$`Day1 Only`[which(is.na(contacts_daily$`Day1 Only`))] <- 0
contacts_daily$`Day2 Only`[which(is.na(contacts_daily$`Day2 Only`))] <- 0
contacts_daily$`NA`[which(is.na(contacts_daily$`NA`))] <- 0
contacts_daily$avg_daily_contacts <- ((contacts_daily$`Both Days` * 2)+
                                          contacts_daily$`Day1 Only` + 
                                          contacts_daily$`Day2 Only`+
                                          contacts_daily$`NA`)/2

median_daily_site <- contacts_daily %>%
  dplyr::group_by(study_site) %>%
  dplyr::summarize(q25 = quantile(avg_daily_contacts, probs = 0.25),
                   q50 = quantile(avg_daily_contacts, probs = 0.5),
                   q75 = quantile(avg_daily_contacts, probs = 0.75))

median_daily <- contacts_daily %>%
  ungroup() %>%
  dplyr::summarize(q25 = quantile(avg_daily_contacts, probs = 0.25),
                   q50 = quantile(avg_daily_contacts, probs = 0.5),
                   q75 = quantile(avg_daily_contacts, probs = 0.75))

mean_daily_site <- contacts_daily %>%
  dplyr::group_by(study_site) %>%
  dplyr::summarize(mean = round(mean(avg_daily_contacts),1))

mean_daily <- contacts_daily %>%
  ungroup() %>%
  dplyr::summarize(mean = round(mean(avg_daily_contacts),1))

ci_contacts_rural <- contacts_daily %>%
  dplyr::filter(study_site == "Rural")
ci_contacts_rural <- lm(avg_daily_contacts ~ 1, ci_contacts_rural)
ci_contacts_rural <- as.data.frame(round(confint(ci_contacts_rural),1))

ci_contacts_urban <- contacts_daily %>%
  dplyr::filter(study_site == "Urban")
ci_contacts_urban <- lm(avg_daily_contacts ~ 1, ci_contacts_urban)
ci_contacts_urban <- as.data.frame(round(confint(ci_contacts_urban),1))

ci_contacts <- contacts_daily
ci_contacts <- lm(avg_daily_contacts ~ 1, contacts_daily)
ci_contacts <- as.data.frame(round(confint(ci_contacts),1))

fitn1 <- fitdistrplus::fitdist(contacts_daily$avg_daily_contacts, "norm")
summary(fitn1)
plot(fitn1)

# fitp1 <- fitdistrplus::fitdist(contacts_daily$avg_daily_contacts, "pois")
# summary(fitp1)
# plot(fitp1)

# fitnb1 <- fitdistrplus::fitdist(contacts_daily$avg_daily_contacts, "nbinom")
# summary(fitnb1)
# plot(fitnb1)

ggplot()+
  geom_histogram(data=contacts_daily, aes(x = avg_daily_contacts,y = ..density..), 
                 color = cols[2], 
                 fill = cols[2])+
  geom_vline(aes(xintercept = mean_daily$mean))+
  geom_vline(aes(xintercept = median_daily$q50), col = "grey")+
  stat_function(fun = dnorm, args = list(mean = fitn1$estimate[1], sd = fitn1$estimate[2]), aes(lty="Normal"))+
  # stat_function(fun = dpois, args = list(lambda = fitp1$estimate[1]), aes(lty="Poisson"))+
  # stat_function(fun = dnbinom, args = list(size = fitnb1$estimate[1], mu = fitnb1$estimate[2], aes(lty = "Neg Binomial")))+
  theme_minimal()+
  scale_colour_manual(name = "Fitted Distributions", values = c(1, 2, 3))+
  labs(x = "Average Daily Unique Contacts", y = "Density", 
       lty = "Fitted Distributions", 
       title = "Average Daily Unique Contacts at Both Sites with Distributions")

#URBAN
urban <- contacts_daily %>% filter(study_site == "Urban")

fitn1 <- fitdistrplus::fitdist(urban$avg_daily_contacts, "norm")
summary(fitn1)
plot(fitn1)

# fitp1 <- fitdistrplus::fitdist(urban$avg_daily_contacts, "pois")
# summary(fitp1)
# plot(fitp1)

# fitnb1 <- fitdistrplus::fitdist(urban$avg_daily_contacts, "nbinom")
# summary(fitnb1)
# plot(fitnb1)

ggplot()+
  geom_histogram(data=urban, aes(x = avg_daily_contacts,y = ..density..), 
                 color = cols[5], 
                 fill = cols[5])+
  geom_vline(aes(xintercept = mean_daily_site$mean[2]))+
  geom_vline(aes(xintercept = median_daily_site$q50[2]), col = "grey")+
  stat_function(fun = dnorm, args = list(mean = fitn1$estimate[1], sd = fitn1$estimate[2]), aes(lty="Normal"))+
  # stat_function(fun = dpois, args = list(lambda = fitp1$estimate[1]), aes(lty="Poisson"))+
  # stat_function(fun = dnbinom, args = list(size = fitnb1$estimate[1], mu = fitnb1$estimate[2], aes(lty = "Neg Binomial")))+
  theme_minimal()+
  scale_colour_manual(name = "Fitted Distributions", values = c(1, 2, 3))+
  labs(x = "Average Daily Unique Contacts", y = "Density", 
       lty = "Fitted Distributions", 
       title = "Average Daily Unique Contacts at Urban Site with Distributions")

#RURAL
rural <- contacts_daily %>% filter(study_site == "Rural")

fitn1 <- fitdistrplus::fitdist(rural$avg_daily_contacts, "norm")
summary(fitn1)
plot(fitn1)

# fitp1 <- fitdistrplus::fitdist(rural$avg_daily_contacts, "pois")
# summary(fitp1)
# plot(fitp1)

# fitnb1 <- fitdistrplus::fitdist(rural$avg_daily_contacts, "nbinom")
# summary(fitnb1)
# plot(fitnb1)

ggplot()+
  geom_histogram(data=rural, aes(x = avg_daily_contacts,y = ..density..), 
                 color = cols[3], 
                 fill = cols[3])+
  geom_vline(aes(xintercept = mean_daily$mean[1]))+
  geom_vline(aes(xintercept = median_daily$q50[1]), col = "grey")+
  stat_function(fun = dnorm, args = list(mean = fitn1$estimate[1], sd = fitn1$estimate[2]), aes(lty="Normal"))+
  # stat_function(fun = dpois, args = list(lambda = fitp1$estimate[1]), aes(lty="Poisson"))+
  # stat_function(fun = dnbinom, args = list(size = fitnb1$estimate[1], mu = fitnb1$estimate[2], aes(lty = "Neg Binomial")))+
  theme_minimal()+
  scale_colour_manual(name = "Fitted Distributions", values = c(1, 2, 3))+
  labs(x = "Average Daily UniqueContacts", y = "Density", 
       lty = "Fitted Distributions", 
       title = "Average Daily Unique Contacts at Rural Site with Distributions")


# Average Daily Unique Contacts Models ----------------------------------------------
contact_unique$age <- sprintf("%02s", contact_unique$age)
contact_unique$sex <- contact_unique$participant_sex
contact_unique$site <- contact_unique$study_site

contact_unique <- contact_unique %>%
  group_by(study_site) %>%
  mutate(N = sum(avg_unique_contacts))
contact_unique$s_age <- as.character(as.numeric(contact_unique$age)-median(as.numeric(contact_unique$age)))

negbin_model <- glm.nb(avg_unique_contacts ~ age + sex + site,
                       data = contact_unique %>% distinct())
unique_negbin <- as.data.frame(summary(negbin_model)$coefficients)

negbin_int_model <- glm.nb(avg_unique_contacts ~ age * sex * site,
                       data = contact_unique %>% distinct())
unique_negbin_int <- as.data.frame(summary(negbin_int_model)$coefficients)

poisson_model <- glm(avg_unique_contacts ~ age + sex + site,
                    data = contact_unique %>% distinct(),
                    family = poisson(link = "log"))
unique_poisson <- as.data.frame(summary(poisson_model)$coefficients)

poisson_int_model <- glm(avg_unique_contacts ~ age * sex * site,
                     data = contact_unique %>% distinct(),
                     family = poisson(link = "log"))
unique_poisson_int <- as.data.frame(summary(poisson_int_model)$coefficients)

linear_model <- glm(avg_unique_contacts ~ age + sex + site,
                     data = contact_unique %>% distinct(),
                     family = gaussian(link = "identity"))
unique_linear <- as.data.frame(summary(linear_model)$coefficients)

linear_int_model <- glm(avg_unique_contacts ~ age * sex * site,
                    data = contact_unique %>% distinct(),
                    family = gaussian(link = "identity"))
unique_linear_int <- as.data.frame(summary(linear_int_model)$coefficients)


## Average Daily Contacts Models ------------------------------------------------------
contacts_daily$age <- sprintf("%02s", contacts_daily$age)
contacts_daily$sex <- contacts_daily$participant_sex
contacts_daily$site <- contacts_daily$study_site

negbin_model <- glm.nb(avg_daily_contacts ~ age + sex + site,
                       data = contacts_daily %>% distinct())
daily_negbin <- as.data.frame(summary(negbin_model)$coefficients)

negbin_int_model <- glm.nb(avg_daily_contacts ~ age * sex * site,
                           data = contacts_daily %>% distinct())
daily_negbin_int <- as.data.frame(summary(negbin_int_model)$coefficients)

poisson_model <- glm(avg_daily_contacts ~ age + sex + site,
                     data = contacts_daily %>% distinct(),
                     family = poisson(link = "log"))
daily_poisson <- as.data.frame(summary(poisson_model)$coefficients)

poisson_int_model <- glm(avg_daily_contacts ~ age * sex * site,
                         data = contacts_daily %>% distinct(),
                         family = poisson(link = "log"))
daily_poisson_int <- as.data.frame(summary(poisson_int_model)$coefficients)

linear_model <- glm(avg_daily_contacts ~ age + sex + site,
                    data = contacts_daily %>% distinct(),
                    family = gaussian(link = "identity"))
daily_linear <- as.data.frame(summary(linear_model)$coefficients)

linear_int_model <- glm(avg_daily_contacts ~ age * sex * site,
                        data = contacts_daily %>% distinct(),
                        family = gaussian(link = "identity"))
daily_linear_int <- as.data.frame(summary(linear_int_model)$coefficients)

# save data --------------------------------------------------------------
saveRDS(contact_unique, "outlier-analysis/data/contact_unique.RDS")
saveRDS(contacts_daily, "outlier-analysis/data/contacts_daily.RDS")

write.csv(unique_linear, "outlier-analysis/data/unique_linear.csv")
write.csv(unique_linear_int, "outlier-analysis/data/unique_linear_int.csv")
write.csv(unique_poisson, "outlier-analysis/data/unique_poisson.csv")
write.csv(unique_poisson_int, "outlier-analysis/data/unique_poisson_int.csv")
write.csv(unique_negbin, "outlier-analysis/data/unique_negbin.csv")
write.csv(unique_negbin_int, "outlier-analysis/data/unique_negbin_int.csv")

write.csv(daily_linear, "outlier-analysis/data/daily_linear.csv")
write.csv(daily_linear_int, "outlier-analysis/data/daily_linear_int.csv")
write.csv(daily_poisson, "outlier-analysis/data/daily_poisson.csv")
write.csv(daily_poisson_int, "outlier-analysis/data/daily_poisson_int.csv")
write.csv(daily_negbin, "outlier-analysis/data/daily_negbin.csv")
write.csv(daily_negbin_int, "outlier-analysis/data/daily_negbin_int.csv")
