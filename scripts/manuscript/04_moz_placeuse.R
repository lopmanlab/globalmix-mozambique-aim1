###############################################################################  
# This file contains scripts to analyze and visualize place use data.
# Author: Carol Liu
# 
###############################################################################  

location <- readRDS("../../data/clean/moz_locations_visited_aim1.RDS") %>%
  filter(study_day == 1)

location <- location %>% 
  dplyr::mutate(
    time_visited_cat = case_when(
      time_visited %in% c("<5 mins","5-15 mins") ~ "<15 mins",
      time_visited %in% c("16-30 mins","31 mins-1 hr") ~ "15 mins-1 hr",
      TRUE~ time_visited)) %>%
  filter(!is.na(place_visited)) %>% # dropped 2 NAs
  filter(!is.na(time_visited)) %>%
  mutate(
    time_visited_cat = factor(time_visited_cat, 
                              levels = c("<15 mins","15 mins-1 hr","1-4 hrs",">4 hrs")),
    
    place_visited = factor(place_visited, 
                           levels = c("My home","Other home","Street","Market/Shop",
                                      "Transport/Hub", "Agricultural Field", "School",
                                      "Work", "Place of worship", "Well", 
                                      "Playground", "Other"))
    )

# figure included in ms
fig4_locvisit_timespent <- location %>%
  group_by(study_site, place_visited, time_visited_cat) %>%
  filter(place_visited!="My home") %>%
  summarise(place_time = n()) %>%
  group_by(study_site, place_visited) %>%  # Group by site and place
  mutate(place_time_proportion = round((place_time / sum(place_time)) * 100), 0) %>%  # Calculate proportions
  ggplot(aes(x = place_visited, y = place_time_proportion, fill = time_visited_cat)) +
  geom_col(position = position_stack(), col="white") +
  # # Remove the "NA" label from the legend
  guides(color = guide_legend(override.aes = list(label = ""))) +
  facet_wrap(~study_site) +
  axis_text_theme2 +
  coord_flip() +
  # scale_fill_brewer(palette="Blues", name="Time spent") +
  scale_fill_manual(values = c("#ccebc5", "#7bccc4", "#43a2ca", "#0868ac"), name="Time spent") +
  theme(axis.text.x = element_text(angle=45,vjust=1, hjust=1))+
  ylab("Proportion of unique visits over 2 days")+
  xlab("Location") +
  theme(axis.text.x = element_text(angle=0)) +
  theme(legend.position = "right",
        legend.text = element_text(size=6),
        legend.direction  = "vertical")
fig4_locvisit_timespent

# ggsave(fig4_locvisit_timespent, filename = "../../output/figs/fig_locvisit_timespent.pdf",
#        height=4, width=6, dpi=300,
#        bg="#FFFFFF")

tab1_loc <- location %>%
  mutate(num_pax_place = as.numeric(num_pax_place)) %>%
  group_by(place_visited, study_site)%>%
  arrange(place_visited) %>%
  summarise(n = n(),
            ppl_med = median(num_pax_place, na.rm=T),
            ppl_25 = quantile(num_pax_place, probs=0.25, na.rm=T),
            ppl_75 = quantile(num_pax_place, probs=0.75, na.rm=T)) %>%
  filter(place_visited != "My home") %>%
  filter(!is.na(study_site) & !is.na(place_visited))%>%
  pivot_wider(names_from = study_site, values_from = c(n, ppl_med, ppl_25, ppl_75))%>%
  mutate(med_Rural = paste(ppl_med_Rural, "(", ppl_25_Rural, "-",ppl_75_Rural,")",sep=""),
         med_Urban = paste(ppl_med_Urban, "(", ppl_25_Urban, "-",ppl_75_Urban, ")",sep="")) %>%
  select(place_visited, n_Rural, med_Rural, n_Urban, med_Urban)

loc_label <- data.frame(
  seq=seq(from=0,to=11, by=1),
  text = "location_contact___",
  place_visited=c("My home","Other home","School","Work","Transport/Hub", 
                  "Market/Shop","Street","Well","Agricultural Field",
                  "Playground", "Place of worship","Other")) %>%
  mutate(place = paste(text,seq,sep="")) %>%
  select(-c(text))

# number of contacts recorded at location
cont_sum <- contacts %>%
  select(rec_id, study_day, location_contact___0:location_contact___11) %>%
  filter(study_day == 1) %>% # new addition 03252024, to be consistent with location data
  pivot_longer(cols=location_contact___0:location_contact___11, names_to="place",values_to="val")%>%
  filter(val==1) %>%
  left_join(loc_label %>% select(place, place_visited))%>%
  group_by(study_day, rec_id, place_visited)%>%
  summarise(social_contact = n())

# number of people in location
loc_sum <- location %>%
  group_by(rec_id, place_visited, study_day, study_site) %>%
  mutate(num_pax_place = as.numeric(num_pax_place)) %>%
  summarise(unique_places = n(),
            tot_persons = sum(num_pax_place))

cont_loc_sum <- loc_sum %>%
  select(-study_site)%>%
  left_join(cont_sum,  
            by = c("rec_id"="rec_id",
                   "study_day"="study_day",
                   "place_visited"="place_visited")) %>%
  
  left_join(participants %>% 
              select(rec_id, study_site), by=c("rec_id"="rec_id")) %>%
  
  mutate(social_contact = ifelse(is.na(social_contact), 0,
                                 social_contact),
         place_visited =    factor(place_visited, 
                                   levels = c("My home","Other home","Street",
                                              "Market/Shop","Transport/Hub", 
                                              "Agricultural Field", "School",
                                              "Work", "Other", "Place of worship", 
                                              "Well", "Playground", NA))) %>%
  pivot_longer(cols = tot_persons:social_contact, 
               values_to="num_persons", 
               names_to="type") %>%
  
  mutate(type = case_when(type == "tot_persons" ~ "People present",
                          type == "social_contact" ~ "Num. of contacts"))

figsi_compdist_urb <- cont_loc_sum %>%
  filter(study_site=="Urban")%>%
  ggplot(aes(x=type, y =num_persons, fill=type))+
  geom_violin(draw_quantiles=c(0.5))+
  scale_y_log10()+
  facet_wrap(~place_visited)+
  # scale_fill_brewer(palette="PuRd",name="")+
  scale_fill_manual(values = c("#35978f", "#c7eae5")) +
  ylab("N")+
  xlab("") + #theme_bw() +
  axis_text_theme2 +
  theme(axis.text.x = element_blank(), #element_text(angle=45,vjust=1, hjust=1),
        axis.title.y = element_text(angle = 0),
        legend.direction = "vertical",
        legend.position = "right")
# figsi_compdist_urb
# ggsave(figsi_compdist_urb, filename = "output/figs/figS2_urban_locations_contacts.pdf",
#        height=6, width=8, dpi=300,
#        bg="#FFFFFF")

figsi_compdist_rur <- cont_loc_sum%>%
  filter(study_site=="Rural" & !is.na(place_visited))%>%
  ggplot(aes(x=type, y =num_persons, fill=type))+
  geom_violin(draw_quantiles=c(0.5))+
  scale_y_log10()+
  facet_wrap(~place_visited)+
  # scale_fill_brewer(palette="PuRd",name="")+
  scale_fill_manual(values = c("#bf812d", "#f6e8c3")) +
  ylab("N")+
  xlab("") +
  axis_text_theme2 +
  theme(axis.text.x = element_blank(), # element_text(angle=45,vjust=1, hjust=1),
        axis.title.y = element_text(angle = 0),
        legend.direction = "vertical",
        legend.position = "right")
# figsi_compdist_rur
# ggsave(figsi_compdist_rur, filename = "output/figs/figS1_rural_locations_contacts.pdf",
#        height=6, width=8, dpi=300,
#        bg="#FFFFFF")


figsi_compdist_combined <- wrap_plots(figsi_compdist_rur, figsi_compdist_urb) + 
  plot_annotation(tag_levels = 'A') + 
  theme(plot.tag = element_text(size = 12)) +
  plot_layout(nrow=2, heights = c(1200))
figsi_compdist_combined
ggsave(figsi_compdist_combined, filename = "output/figs/figS1_number_combined.pdf",
       height=9, width=8, dpi=300,
       bg="#FFFFFF")

figsi_compdist_rur_all <- cont_loc_sum %>%
  filter(!is.na(place_visited))%>%
  ggplot(aes(x=type, y =num_persons, fill=type))+
  geom_violin(draw_quantiles=c(0.5))+
  scale_y_log10()+
  facet_wrap(~place_visited) +
  scale_fill_brewer(palette="PuRd",name="")+
  ylab("Distribution of persons (log scale)")+
  xlab("") + 
  axis_text_theme2 +
  theme(axis.text.x = element_text(angle=45,vjust=1, hjust=1),
        legend.direction = "vertical",
        legend.position = "right")
figsi_compdist_rur_all

