library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)
library(cowplot)

## Read in data
ind <- readRDS("../../data/clean/participant_data_aim1.RDS")
ind_exit <- readRDS("../../data/clean/exit_interview_aim1.RDS")
contact <- readRDS("../../data/clean/contact_data_aim1.RDS")
#contact_nonhh <- readRDS("../data/clean/ind_contact_nonhh.RDS")
hh_survey <- readRDS("../../data/clean/household_survey_aim1.RDS")
loc <- readRDS("../../data/clean/locations_visited_aim1.RDS") ## loc clean
#loc_orig <- readRDS("../data/clean/old_data/ind_location_visit.RDS")
#loc_orig2 <- readRDS("../data/clean/old_data/ind_location_visit2.RDS")

loc <- loc %>% mutate(
  time_visited_cat = case_when(
    time_visited%in%c("<5 mins","5-15 mins")~ "<15 mins",
    time_visited%in%c("16-30 mins","31 mins-1 hr")~ "15 mins-1 hr",
    TRUE~ time_visited
  ))%>%
  mutate(
    time_visited_cat = factor(time_visited_cat, levels = c("<15 mins","15 mins-1 hr","1-4 hrs",">4 hrs")),
    place_visited =    factor(place_visited, levels = c("My home","Other home","Street","Market/Shop",
                                                        "Transport/Hub", "Agricultural Field", "School",
                                                        "Work", "Other", "Place of worship", "Well", "Playground", NA))
  )

fig4_locvisit_timespent <- loc%>%
  group_by(study_site, place_visited,time_visited_cat)%>%
  filter(place_visited!="My home")%>%
  summarise(place_time = n())%>%
  #pivot_wider(names_from="time_visited",values_from = "place_time")%>%
  ggplot(aes(x=place_visited, y=place_time,fill=time_visited_cat))+
  geom_col(position=position_stack())+
  facet_wrap(~study_site)+theme_bw()+
  scale_fill_brewer(palette="Blues", name="Time spent")+
  theme(axis.text.x = element_text(angle=45,vjust=1, hjust=1))+
  ylab("Number of unique visits over 2 days")+
  xlab("Type of location")

tab1_loc <- loc %>%
  mutate(num_pax_place=as.numeric(num_pax_place))%>%
  group_by(place_visited, study_site)%>%
  arrange(place_visited)%>%
  summarise(n=n(),
            ppl_med = median(num_pax_place, na.rm=T),
            ppl_25 = quantile(num_pax_place, probs=0.25, na.rm=T),
            ppl_75 = quantile(num_pax_place, probs=0.75, na.rm=T))%>%
  filter(place_visited != "My home")%>%
  filter(!is.na(study_site)&!is.na(place_visited))%>%
  pivot_wider(names_from = study_site, values_from = c(n, ppl_med, ppl_25, ppl_75))%>%
  mutate(med_Rural = paste(ppl_med_Rural, "(", ppl_25_Rural, "-",ppl_75_Rural,")",sep=""),
         med_Urban = paste(ppl_med_Urban, "(", ppl_25_Urban, "-",ppl_75_Urban, ")",sep=""))%>%
  select(place_visited, n_Rural, med_Rural, n_Urban, med_Urban)

loc_label<-data.frame(
  seq=seq(from=0,to=11, by=1),
  text = "location_contact___",
  place_visited=c("My home","Other home","School","Work","Transport/Hub","Market/Shop","Street","Well","Agricultural Field",
                  "Playground", "Place of worship","Other")
) %>%
  mutate(place = paste(text,seq,sep=""))

cont_sum <- contact%>%select(rec_id,study_day, location_contact___0:location_contact___11)%>%
  pivot_longer(cols=location_contact___0:location_contact___11, names_to="place",values_to="val")%>%
  filter(val==1)%>%
  left_join(loc_label%>%select(place, place_visited))%>%
  group_by(study_day, rec_id, place_visited)%>%
  summarise(social_contact=n())

loc_sum <- loc%>%
  group_by(rec_id, place_visited, study_day,study_site)%>%
  mutate(num_pax_place=as.numeric(num_pax_place))%>%
  summarise(unique_places = n(),
            tot_persons=sum(num_pax_place))

cont_loc_sum <- loc_sum %>%
  select(-study_site)%>%
  left_join(cont_sum,  
            by = c("rec_id"="rec_id",
                   "study_day"="study_day",
                   "place_visited"="place_visited"))%>%
  left_join(participants %>% select(rec_id, study_site), by=c("rec_id"="rec_id"))%>%
  
  mutate(social_contact = ifelse(is.na(social_contact),0,social_contact),
         place_visited =    factor(place_visited, levels = c("My home","Other home","Street","Market/Shop",
                                                             "Transport/Hub", "Agricultural Field", "School",
                                                             "Work", "Other", "Place of worship", "Well", "Playground", NA)))%>%
  pivot_longer(cols=tot_persons:social_contact, values_to="num_persons",names_to="type")

figsi_compdist_urb <- cont_loc_sum%>%
  filter(study_site=="Urban")%>%
  ggplot(aes(x=type, y =num_persons, fill=type))+
  geom_violin(draw_quantiles=c(0.5))+
  scale_y_log10()+
  facet_wrap(~place_visited)+
  scale_fill_brewer(palette="PuRd",name="")+
  ylab("Distribution of persons")+
  xlab("")+theme_bw()+
  theme(axis.text.x = element_text(angle=45,vjust=1, hjust=1))


figsi_compdist_rur <- cont_loc_sum%>%
  filter(study_site=="Rural" & !is.na(place_visited))%>%
  ggplot(aes(x=type, y =num_persons, fill=type))+
  geom_violin(draw_quantiles=c(0.5))+
  scale_y_log10()+
  facet_wrap(~place_visited)+
  scale_fill_brewer(palette="PuRd",name="")+
  ylab("Distribution of persons (log scale)")+
  xlab("")+theme_bw()+
  theme(axis.text.x = element_text(angle=45,vjust=1, hjust=1))

figsi_compdist_rur_all <- cont_loc_sum%>%
  filter(!is.na(place_visited))%>%
  ggplot(aes(x=type, y =num_persons, fill=type))+
  geom_violin(draw_quantiles=c(0.5))+
  scale_y_log10()+
  facet_wrap(~place_visited)+
  scale_fill_brewer(palette="PuRd",name="")+
  ylab("Distribution of persons (log scale)")+
  xlab("")+theme_bw()+
  theme(axis.text.x = element_text(angle=45,vjust=1, hjust=1))