
###############################################################################  
# This file contains the analysis code.
# Author: Moses C Kiti, PhD
###############################################################################

participants <- readRDS("../../data/clean/participant_data_aim1.RDS")
# exit <- readRDS("../../data/clean/exit_interview_aim1.RDS")
contacts <- readRDS("../../data/clean/contact_data_aim1.RDS")
household <- readRDS("../../data/clean/household_survey_aim1.RDS")
location <- readRDS("../../data/clean/locations_visited_aim1.RDS")
# 

# set color themes
# cols_site <- c("#1f77b4", "#ff7f0e")
cols_site <- c("#d8b365", "#5ab4ac")
cols_sex <- c("#9467bd", "#aec7e8") #c ("#ef8a62", "#67a9cf")
cols_week <- c("#8c564b", "#7f7f7f", "#d62728", "#17becf", "#e377c2", "#bcbd22", "#800080")



# household-characteristics
#| include: false
#| warning: false
#| echo: false

mean_hh_size <- participants %>%
  dplyr::group_by (study_site) %>%
  dplyr::summarize(mean_hh_size = round(mean(hh_size %>% na.omit()), 1)) # omits 1
# check range of household size
# rur_hhsize <- participants %>% filter(study_site=="Rural"); range(rur_hhsize$hh_size)
# rur_hhsize <- participants %>% filter(study_site=="Rural"); range(rur_hhsize$hh_size)

participants <- participants %>%
  dplyr::mutate(hh_size_cat = dplyr::case_when(hh_size == 1 ~ "1",
                                               hh_size == 2 | hh_size == 3 ~ "2-3",
                                               hh_size == 4 | hh_size == 6 ~ "4-6",
                                               hh_size == 7 | hh_size == 10 ~ "7-10",
                                               hh_size >10 ~ "10+"),
                hh_size_cat = factor(hh_size_cat,
                                     levels = c("1", "2-3", "4-6", "7-10", "10+")))

# combine some occupation categories
# additional-cleaning-contacts


# cleaning contacts
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
  mutate(contact_mask2 = dplyr::case_when(contact_mask == "Yes, for the entire encounter" ~ "Yes",
                                   contact_mask == "Yes, during parts of encounter" ~ "Yes",
                                   contact_mask == "No mask was worn during the encounter" ~ "No",
                                   TRUE ~ "I don't remember"))
contacts$contact_mask2 <- factor(contacts$contact_mask2, levels = c("Yes", "No",
                                                                    "I don't remember"))

contacts$cnt_home <- factor(contacts$cnt_home, levels = c(1, 0),
                            labels = c("Yes", "No"))
contacts$cnt_work <- factor(contacts$cnt_work, levels = c(1, 0),
                            labels = c("Yes", "No"))
contacts$cnt_school <- factor(contacts$cnt_school, levels = c(1, 0),
                              labels = c("Yes", "No"))
contacts$cnt_otherplace <- factor(contacts$cnt_otherplace, levels = c(1, 0),
                                  labels = c("Yes", "No"))

# combine some contact time categories
contacts <- contacts %>%
  mutate(duration_contact2 = as.character(duration_contact),
         duration_contact2 = dplyr::case_when(duration_contact2 == "5-15 mins" ~ "5-30 mins",
                                       duration_contact2 == "16-30 mins" ~ "5-30 mins",
                                       duration_contact2 == "1-4 hrs" ~ ">1 hr",
                                       duration_contact2 == ">4 hrs" ~ ">1 hr",
                                       TRUE ~ duration_contact2),
         duration_contact2 = factor(duration_contact2,
                                    levels = c("<5 mins", "5-30 mins", "31 mins-1 hr",
                                               ">1 hr"),
                                    labels = c("<5mins", "5-30mins", "31mins-1hr",
                                               ">1hr")))

# generate data strata by COVID-19 period
## Looks like some issues with this where surveydate not matching exactly with Day 1 or2
# first_survey_date <- contacts %>%
#   dplyr::select(rec_id, "survey_date") %>%
#   dplyr::arrange(rec_id, "survey_date") %>%
#   dplyr::group_by(rec_id) %>%
#   slice(n=1) %>%
#   dplyr::rename(dt_diaryd1 = survey_date) %>%
#   mutate(pand_period = dplyr::case_when(dt_diaryd1 <= as.Date("2021-6-1") ~ "lull1",
#                                  dt_diaryd1 <= as.Date("2021-7-31") ~ "inc_delta",
#                                  dt_diaryd1 <= as.Date("2021-9-28") ~ "dec_delta",
#                                  dt_diaryd1 <= as.Date("2021-12-8") ~ "lull2",
#                                  dt_diaryd1 <= as.Date("2022-1-6") ~ "inc_omi",
#                                  dt_diaryd1 <= as.Date("2022-2-6") ~ "dec_omi",
#                                  dt_diaryd1 <= as.Date("2022-6-1") ~ "lull3",
#                                  TRUE ~ NA_character_),
#          pand_period_simp = dplyr::case_when(dt_diaryd1 <= as.Date("2021-6-1") ~ "lull",
#                                       dt_diaryd1 <= as.Date("2021-7-31") ~ "wave",
#                                       dt_diaryd1 <= as.Date("2021-9-28") ~ "wave",
#                                       dt_diaryd1 <= as.Date("2021-12-8") ~ "lull",
#                                       dt_diaryd1 <= as.Date("2022-1-6") ~ "wave",
#                                       dt_diaryd1 <= as.Date("2022-2-6") ~ "wave",
#                                       dt_diaryd1 <= as.Date("2022-6-1") ~ "lull",
#                                       TRUE ~ NA_character_),
#          
#          pand_period_simp1 = dplyr::case_when(
#            dt_diaryd1 <= as.Date("2021-6-1") ~ "lull",
#            dt_diaryd1 <= as.Date("2021-7-31") ~ "inc",
#            dt_diaryd1 <= as.Date("2021-9-28") ~ "dec",
#            dt_diaryd1 <= as.Date("2021-12-8") ~ "lull",
#            dt_diaryd1 <= as.Date("2022-1-6") ~ "inc",
#            dt_diaryd1 <= as.Date("2022-2-6") ~ "dec",
#            dt_diaryd1 <= as.Date("2022-6-1") ~ "lull",
#            TRUE ~ NA_character_ ))

# total number of participants per site
N <- nrow(participants)
N_site <- participants %>%
  group_by(study_site, participant_sex) %>%
  summarize(N = n())

# total number of contacts
tot_contacts <- nrow(contacts)
tot_contacts_site <- contacts %>%
  group_by(study_site) %>%
  summarize(freq = n())
tot_rural_contacts <- nrow(contacts %>% filter(study_site == "Rural"))
tot_urban_contacts <- nrow(contacts %>% filter(study_site == "Urban"))


#| label: tbl-participant-summary
label(participants$study_site) <- "Site"
label(participants$aim) <- "Aim"
label(participants$participant_sex) <- "Sex"
# label(participants$age) <- "Exact age"
label(participants$participant_age) <- "Participant age"
label(participants$read_write) <- "Able to read and write"
label(participants$enrolled_school) <- "Currently enrolled in school"
label(participants$highest_educ) <- "Highest education level attained"
label(participants$school_level) <- "Current school level"
# label(participants$transport_use) <- "Transport use last 3 months"
label(participants$occupation) <- "Occupation"
label(participants$mask_use) <- "Regular mask use"
label(participants$age_symptom) <- "Acute gastroenteritis (diarrhea)"
label(participants$ari_symptom) <- "Acute respiratory infection"
label(participants$contacts_completedby) <- "Who filled the diary?"
label(participants$all_contacts) <- "Did you record all contacts?"
label(participants$behavior_change) <- "Any social behavior due to the pandemic?"

table1 <- participants %>%
  # dplyr::filter(aim==1) %>%
  dplyr::select(study_site, participant_sex, participant_age,  read_write, 
                enrolled_school, occupation, mask_use, age_symptom, 
                ari_symptom, contacts_completedby) %>% # day_of_week, year, study_site
  tbl_summary(by=study_site, 
              percent="column",
              digits = all_categorical() ~ 0,
              missing="ifany") %>%
  add_overall() %>%
  bold_labels() %>%
  modify_header(label = "") %>%
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Site**") 
# %>% modify_caption("**Table 1: Baseline characteristics of participants in Mozambique**")

# check homemaker, college

# ## save table as text
# table1



#| label: fig-participant-summary-agesex
# Sex by participant_age
sex_page <- participants %>% 
  group_by(participant_sex, participant_age, study_site) %>% 
  dplyr::summarise(total = n(), .groups = "drop") %>%
  na.omit()

fig_sex_age <- sex_page %>%
  ggplot(., aes(x=participant_age, y=total, fill=participant_sex)) +
  geom_bar(aes(x = participant_age, y = total, fill = participant_sex), 
           position = "stack", col="white", stat="identity") +
  scale_fill_manual(values = cols_sex) +  # Set custom fill colors
  facet_wrap(~study_site) +
  annotation_custom(grobTree(textGrob("Target=63", x=0.5, y=0.7, hjust=-0.2,
                                      gp=gpar(fontsize=8)))) +
  coord_flip() +
  
  labs(title = "",
       x = "Participant age", 
       y = "N",
       fill = element_blank()) +
  axis_text_theme2 +
  theme(legend.position = c(0.4, 0.4),
        legend.key.size = unit(0.5, "lines"),
        strip.background = element_blank(),
        legend.direction  = "vertical",
        # strip.text = element_blank(), # remove facet titles
        panel.spacing = unit(2, "lines")) +
  geom_hline(yintercept=63, linetype="dashed")

# fig_sex_age
ggsave(fig_sex_age, filename = "../../output/figs/fig1_participant_age_sex.pdf",
       height=6, width=10, dpi=300,
       bg="#FFFFFF")


#| label: fig-diary-filler
#| include: false

# diary filler by participant_age
diary_filler <- participants %>% 
  group_by(contacts_completedby, participant_age, study_site) %>% 
  dplyr::summarise(total = n(), .groups = "drop") %>%
  na.omit() %>%
  group_by(study_site) %>%  # Group by study_site
  mutate(proportion = total/sum(total))  # Calculate proportion within each study_site


fig_diary_filler <- diary_filler %>%
  ggplot(aes(x = participant_age, y = proportion, fill = contacts_completedby)) +
  geom_bar(stat="identity", position = "fill", col = "white") +
  geom_text(aes(label = scales::percent(proportion)),  # Provide the label aesthetic
            position = position_fill(vjust = 0.5), show.legend = FALSE) +
  facet_wrap(~study_site) +
  coord_flip() +
  
  labs(title = "",
       x = "Participant age", 
       y = "N",
       fill = element_blank()) +
  axis_text_theme2 +
  theme(legend.position = c(0.4, 0.2),
        legend.key.size = unit(0.5, "lines"),
        strip.background = element_blank(),
        legend.direction  = "vertical",
        # strip.text = element_blank(), # remove facet titles
        panel.spacing = unit(2, "lines"))

# fig_diary_filler

# merge participant and contact data
df_contact <- contacts %>%
  dplyr::select(-study_site) %>%
  left_join(participants, by=("rec_id"))

# overall number of contacts
contacts_overall <- df_contact %>%
  group_by(rec_id) %>%
  dplyr::summarize(num_contacts = n(), .groups = "drop")

# median (IQR) limits
q <- c(.25, .5, .75)

median_contacts_overall <- contacts_overall %>%
  dplyr::summarize(q25 = quantile(num_contacts, probs = q[1]),
                   q50 = quantile(num_contacts, probs = q[2]),
                   q75 = quantile(num_contacts, probs = q[3]),
                   .groups = "drop")

mean_contacts_overall <- contacts_overall %>%
  dplyr::summarize(mean = round(mean(num_contacts),1), .groups = "drop")

ci_contacts_overall <- lm(num_contacts ~ 1, contacts_overall)
ci_contacts_overall <- as.data.frame(round(confint(ci_contacts_overall), 1))
names(ci_contacts_overall)[1] <- "ll"
names(ci_contacts_overall)[2] <- "ul"



# SUMMARY OF CONTACTS BY SITE
# rural contacts
df_contact_rural <- df_contact %>%
  dplyr::filter(study_site == "Rural")

# urban contacts
df_contact_urban <- df_contact %>%
  dplyr::filter(study_site == "Urban")


# contacts by site
contacts_site <- df_contact %>%
  dplyr::group_by(rec_id, study_site) %>%
  dplyr::summarize(num_contacts = n(), .groups = "drop")

# median (IQR) limits
q <- c(.25, .5, .75)

median_contacts_site <- contacts_site %>%
  dplyr::group_by(study_site) %>%
  dplyr::summarize(q25 = quantile(num_contacts, probs = q[1]),
                   q50 = quantile(num_contacts, probs = q[2]),
                   q75 = quantile(num_contacts, probs = q[3]),
                   .groups = "drop")

mean_contacts_site <- contacts_site %>%
  dplyr::group_by(study_site) %>%
  dplyr::summarize(mean = round(mean(num_contacts),1), 
                   .groups = "drop")


ci_contacts_rural <- contacts_site %>%
  dplyr::filter(study_site == "Rural")
ci_contacts_rural <- lm(num_contacts ~ 1, ci_contacts_rural)
ci_contacts_rural <- as.data.frame(round(confint(ci_contacts_rural), 1))
names(ci_contacts_rural)[1] <- "ll"
names(ci_contacts_rural)[2] <- "ul"

ci_contacts_urban <- contacts_site %>%
  dplyr::filter(study_site == "Urban")
ci_contacts_urban <- lm(num_contacts ~ 1, ci_contacts_urban)
ci_contacts_urban <- as.data.frame(round(confint(ci_contacts_urban), 1))
names(ci_contacts_urban)[1] <- "ll"
names(ci_contacts_urban)[2] <- "ul"


#| eval: true
#| label: fig-contact-distribution-overall-plotly

# # rural histogram
hist_rural <- contacts_site %>%
  dplyr::filter(study_site == "Rural") %>%
  ggplot(., aes(x=num_contacts)) +
  geom_histogram(color = cols_site[1], fill = cols_site[1], binwidth = 1) +
  # scale_fill_manual(values = cols_site[1]) +
  scale_x_continuous(limits = c(0, 60)) +
  geom_vline(xintercept=mean_contacts_site$mean[1], color="black") +
  annotate("text", x=mean_contacts_site$mean[1]+2, y=55,
           label=paste0(mean_contacts_site$mean[1],
                        " (95% CI ", ci_contacts_rural$ll,"-",
                        ci_contacts_rural$ul,")"),
           hjust=0, size=4) +
  labs(title = "Rural",
       x="",
       y="Freq.") +
  theme(plot.title = element_text(size = 14),
        axis.title.x = element_text(size=12, face="bold"),
        axis.title.y = element_text(size=12, face="bold"),
        axis.text.x = element_text(size = 8, angle=0, hjust = 1),
        axis.text.y = element_text(size= 8),
        legend.title = element_blank()) +
  theme_classic() +
  axis_text_theme2
# hist_rural

## urban histogram
hist_urban <- contacts_site %>%
  dplyr::filter(study_site == "Urban") %>%
  ggplot(., aes(x=num_contacts)) +
  geom_histogram(color = cols_site[2], fill = cols_site[2], binwidth = 1) +
  # scale_fill_manual(values = cols_site[2]) +
  scale_x_continuous(limits = c(0, 60)) +
  geom_vline(xintercept=mean_contacts_site$mean[2], color="black") +
  annotate("text", x=mean_contacts_site$mean[2]+2, y=55,
           label=paste0(mean_contacts_site$mean[2],
                        " (95% CI ", ci_contacts_urban$ll,"-",
                        ci_contacts_urban$ul,")"),
           hjust=0, size=4) +
  labs(title = "Urban",
       x="",
       y="Frequency") +
  theme(plot.title = element_text(size = 14),
        axis.title.x = element_text(size=12, face="bold"),
        axis.title.y = element_text(size=12, face="bold"),
        axis.text.x = element_text(size = 8, angle=0, hjust = 1),
        axis.text.y = element_text(size= 8),
        legend.title = element_blank()) +
  axis_text_theme2
# hist_urban

contact_hist_overall <- hist_rural / hist_urban 

# ggsave(contact_hist_overall, filename = "../output/figs/fig1_contact_hist_overall.pdf",
#        height=6, width=8, dpi=300,
#        bg="#FFFFFF")

# contact_hist_overall

# overall_contacts_hist <- cowplot::plot_grid(hist_rural, hist_urban,
#                                             labels = c("A", "B"),
#                                             label_size = 12,
#                                             label_fontfamily = "sans",
#                                             ncol = 1, align = "hv", axis="tb",
#                                             common.legend = TRUE, legend = "bottom") 
# # save
# # cowplot::save_plot("../output/figs/moz_overall_contact_histogram.pdf", 
# #                    overall_contacts_hist, 
# #                    base_height = 8, base_width = 7)



#| label: contacts-by-day
# number of contacts per participant by study day

# contacts that happened on day 1 only
df_contact_d1 <- df_contact %>%
  dplyr::filter(study_day==1)

day1_num_contacts <- df_contact_d1 %>%
  dplyr::group_by(rec_id, study_site) %>%
  dplyr::summarize(num_contacts = n()) %>%
  dplyr::select(rec_id, num_contacts, study_site)
df_contact_d1 <- left_join(df_contact_d1, day1_num_contacts, 
                           by = c("rec_id","study_site"))

# mean overall number of contacts on day 1 only
ci_contacts_overall_d1 <- lm(num_contacts ~ 1, day1_num_contacts)
mean_contacts_overall_d1 <- round(ci_contacts_overall_d1$coefficients, 1)
#   df_contact_d1 %>%
#   dplyr::summarize(mean = round(mean(num_contacts),1), .groups = "drop")
ci_contacts_overall_d1 <- as.data.frame(round(confint(ci_contacts_overall_d1), 1))
names(ci_contacts_overall_d1)[1] <- "ll"
names(ci_contacts_overall_d1)[2] <- "ul"


# contacts by site on d1
contacts_site_d1 <- df_contact_d1 %>%
  dplyr::group_by(rec_id, study_site) %>%
  dplyr::summarize(num_contacts = n())

median_contacts_site_d1 <- contacts_site_d1 %>%
  dplyr::group_by(study_site) %>%
  dplyr::summarize(q25 = quantile(num_contacts, probs = q[1]),
                   q50 = quantile(num_contacts, probs = q[2]),
                   q75 = quantile(num_contacts, probs = q[3]))

mean_contacts_site_d1 <- contacts_site_d1 %>%
  dplyr::group_by(study_site) %>%
  dplyr::summarize(mean = round(mean(num_contacts),1))

ci_contacts_rural_d1 <- contacts_site_d1 %>%
  dplyr::filter(study_site == "Rural")
ci_contacts_rural_d1 <- lm(num_contacts ~ 1, ci_contacts_rural_d1)
ci_contacts_rural_d1 <- as.data.frame(round(confint(ci_contacts_rural_d1),1))
names(ci_contacts_rural_d1)[1] <- "ll"
names(ci_contacts_rural_d1)[2] <- "ul"

ci_contacts_urban_d1 <- contacts_site_d1 %>%
  dplyr::filter(study_site == "Urban")
ci_contacts_urban_d1 <- lm(num_contacts ~ 1, ci_contacts_urban_d1)
ci_contacts_urban_d1 <- as.data.frame(round(confint(ci_contacts_urban_d1),1))
names(ci_contacts_urban_d1)[1] <- "ll"
names(ci_contacts_urban_d1)[2] <- "ul"


# contacts that happened on day 2 only
df_contact_d2 <- df_contact %>%
  dplyr::filter(study_day==2)

day2_num_contacts <- df_contact_d2 %>%
  dplyr::group_by(rec_id, study_site) %>%
  dplyr::summarize(num_contacts = n()) %>%
  dplyr::select(rec_id, num_contacts)
df_contact_d2 <- left_join(df_contact_d2, day2_num_contacts, by="rec_id" )

# mean overall number of contacts on day 2 only
ci_contacts_overall_d2 <- lm(num_contacts ~ 1, day2_num_contacts)
mean_contacts_overall_d2 <- round(ci_contacts_overall_d2$coefficients, 1)
#   df_contact_d2 %>%
#   dplyr::summarize(mean = round(mean(num_contacts),1), .groups = "drop")
ci_contacts_overall_d2 <- as.data.frame(round(confint(ci_contacts_overall_d2), 1))
names(ci_contacts_overall_d2)[1] <- "ll"
names(ci_contacts_overall_d2)[2] <- "ul"

contacts_site_d2 <- df_contact_d2 %>%
  dplyr::group_by(rec_id, study_site) %>%
  dplyr::summarize(num_contacts = n())

median_contacts_site_d2 <- contacts_site_d2 %>%
  dplyr::group_by(rec_id) %>%
  dplyr::summarize(q25 = quantile(num_contacts, probs = q[1]),
                   q50 = quantile(num_contacts, probs = q[2]),
                   q75 = quantile(num_contacts, probs = q[3]))

mean_contacts_site_d2 <- contacts_site_d2 %>%
  dplyr::group_by(study_site) %>%
  dplyr::summarize(mean = round(mean(num_contacts),1))

ci_contacts_rural_d2 <- contacts_site_d2 %>%
  dplyr::filter(study_site == "Rural")
ci_contacts_rural_d2 <- lm(num_contacts ~ 1, ci_contacts_rural_d2)
ci_contacts_rural_d2 <- as.data.frame(round(confint(ci_contacts_rural_d2),1))
names(ci_contacts_rural_d2)[1] <- "ll"
names(ci_contacts_rural_d2)[2] <- "ul"

ci_contacts_urban_d2 <- contacts_site_d2 %>%
  dplyr::filter(study_site == "Urban")
ci_contacts_urban_d2 <- lm(num_contacts ~ 1, ci_contacts_urban_d2)
ci_contacts_urban_d2 <- as.data.frame(round(confint(ci_contacts_urban_d2),1))
names(ci_contacts_urban_d2)[1] <- "ll"
names(ci_contacts_urban_d2)[2] <- "ul"


### Wilcox rank-sum test for significance in difference of means by day. ###
# Ho: mean number of paired contacts on day 1 is equal to day 2.
# check if the datasets have the same records in rec_id for a paired test. for the
# missing rec_ids in each dataset, append and assign value of 0 (no contacts reported) 
# to each rec_id

nrow(day1_num_contacts) == nrow(day2_num_contacts)
# we see that not.
# so, we check which records are in d1 not in d2 and vice versa, append these and
# assign a value of 0 to num_contacts.
checkd1 <- day1_num_contacts %>%
  filter(!rec_id %in% unlist(day2_num_contacts$rec_id)) %>%
  select(rec_id) %>%
  mutate(num_contacts = 0)

checkd2 <- day2_num_contacts %>%
  filter(!rec_id %in% unlist(day1_num_contacts$rec_id)) %>%
  select(rec_id) %>%
  mutate(num_contacts = 0)

day1_num_contacts <- rbind(day1_num_contacts, checkd2)
day2_num_contacts <- rbind(day2_num_contacts, checkd1)

# Ho: difference in means is equal to 0
ttest_overall_d1d2 <- round(t.test(day1_num_contacts$num_contacts, 
                                   day2_num_contacts$num_contacts,
                                   paired = T)$p.value, 2)
# ttest_overall_d1d2

# Ho: median is the same
wilcoxtest_overall_d1d2 <- round(wilcox.test(day1_num_contacts$num_contacts, 
                                             day2_num_contacts$num_contacts,
                                             paired = T)$p.value, 2)
# wilcoxtest_overall_d1d2
# overall, there is a difference in the number of contacts reported on day 1 vs day 2.


# Wilcoxon rank-sum test comparing rural and urban contacts
rural_day1_contacts <- day1_num_contacts %>%
  filter(study_site == "Rural") %>%
  select(num_contacts)
urban_day1_contacts <- day1_num_contacts %>%
  filter(study_site == "Urban")  %>%
  select(num_contacts)

ttest_bysite_d1 <- round(t.test(rural_day1_contacts $num_contacts, 
                                urban_day1_contacts $num_contacts)$p.value,2)
# ttest_bysite_d1 # p<0.05

wilcoxtest_bysite_d1 <- round(wilcox.test(rural_day1_contacts $num_contacts, 
                                          urban_day1_contacts $num_contacts)$p.value,2)
# wilcoxtest_bysite_d1 # p<0.05

# plot
# overall_contacts_summary <- contacts_site_d1 %>%
#   group_by(study_site) %>%
#   summarize(mean_num_contacts = round(mean(num_contacts), 1), 
#             ci_low = round(mean_num_contacts - 1.96 * sd(num_contacts) / sqrt(n()), 1), 
#             ci_high = round(mean_num_contacts + 1.96 * sd(num_contacts) / sqrt(n()),1))
# 
# ggplot(contacts_site_d1, aes(x=num_contacts)) +
#   geom_histogram(binwidth=1, alpha=0.7) +
#   facet_wrap(~study_site, ncol=1) +
#   geom_vline(data = overall_contacts_summary, aes(xintercept = mean_num_contacts), 
#              linetype="dashed", color="black", size=0.5) +
#   labs(x = "Num of contacts", y = "Count", title = "Number of contacts by study site") +
#   theme_classic() 
# wilcoxtest_bysite_d1 # p<0.05

# plot
# overall_contacts_summary <- contacts_site_d1 %>%
#   group_by(study_site) %>%
#   summarize(mean_num_contacts = round(mean(num_contacts), 1), 
#             ci_low = round(mean_num_contacts - 1.96 * sd(num_contacts) / sqrt(n()), 1), 
#             ci_high = round(mean_num_contacts + 1.96 * sd(num_contacts) / sqrt(n()),1))
# 
# ggplot(contacts_site_d1, aes(x=num_contacts)) +
#   geom_histogram(binwidth=1, alpha=0.7) +
#   facet_wrap(~study_site, ncol=1) +
#   geom_vline(data = overall_contacts_summary, aes(xintercept = mean_num_contacts), 
#              linetype="dashed", color="black", size=0.5) +
#   labs(x = "Num of contacts", y = "Count", title = "Number of contacts by study site") +
#   theme_classic() 


## rural histogram
contact_hist_rural_d1 <- contacts_site_d1 %>%
  dplyr::filter(study_site == "Rural") %>%
  ggplot(., aes(x=num_contacts)) +
  geom_histogram(color = cols_site[1], fill = cols_site[1], binwidth = 1) +
  # scale_fill_manual(values = cols_site[1]) +
  scale_x_continuous(limits = c(0, 40)) +
  scale_y_continuous(limits = c(0, 80)) +
  geom_vline(xintercept=mean_contacts_site_d1$mean[1], color="black") +
  annotate("text", x=mean_contacts_site_d1$mean[1]+1, y=65,
           label=paste0(mean_contacts_site_d1$mean[1],
                        " (", ci_contacts_rural_d1$ll,"-", #" (95% CI ", 
                        ci_contacts_rural_d1$ul,")"),
           hjust=0, size=2.5) +
  labs(title = "Rural", # Rural
       x="",
       y="Freq.") + # Frequency
  axis_text_theme2
# hist_rural

## urban histogram
contact_hist_urban_d1 <- contacts_site_d1 %>%
  dplyr::filter(study_site == "Urban") %>%
  ggplot(., aes(x=num_contacts)) +
  geom_histogram(color = cols_site[2], fill = cols_site[2], binwidth = 1) +
  # scale_fill_manual(values = cols_site[2]) +
  scale_x_continuous(limits = c(0, 40)) +
  geom_vline(xintercept=mean_contacts_site_d1$mean[2], color="black") +
  annotate("text", x=mean_contacts_site_d1$mean[2]+1, y=65,
           label=paste0(mean_contacts_site_d1$mean[2],
                        " (", ci_contacts_urban_d1$ll,"-", # " (95% CI ", 
                        ci_contacts_urban_d1$ul,")"),
           hjust=0, size=2.5) +
  labs(title = "Urban", # Urban
       x="",
       y="") +
  axis_text_theme2
# hist_urban

contact_hist_d1 <- contact_hist_rural_d1 | contact_hist_urban_d1

# ggsave(contact_hist_d1, filename = "../../output/figs/fig_contact_hist_d1.pdf",
#        height=6, width=8, dpi=300,
#        bg="#FFFFFF")


## rural histogram
contact_hist_rural_d2 <- contacts_site_d2 %>%
  dplyr::filter(study_site == "Rural") %>%
  ggplot(., aes(x=num_contacts)) +
  geom_histogram(color = cols_site[1], fill = cols_site[1], binwidth = 1) +
  # scale_fill_manual(values = cols_site[1]) +
  scale_x_continuous(limits = c(0, 40)) +
  scale_y_continuous(limits = c(0, 80)) +
  geom_vline(xintercept=mean_contacts_site_d2$mean[1], color="black") +
  annotate("text", x=mean_contacts_site_d2$mean[1]+1, y=65,
           label=paste0(mean_contacts_site_d2$mean[1],
                        " (95% CI ", ci_contacts_rural_d2$ll,"-",
                        ci_contacts_rural_d2$ul,")"),
           hjust=0, size=4) +
  labs(x="", y="Freq.") +
  theme_classic() +
  axis_text_theme2
# hist_rural

## urban histogram
contact_hist_urban_d2 <- contacts_site_d2 %>%
  dplyr::filter(study_site == "Urban") %>%
  ggplot(., aes(x=num_contacts)) +
  geom_histogram(color = cols_site[2], fill = cols_site[2], binwidth = 1) +
  # scale_fill_manual(values = cols_site[2]) +
  scale_x_continuous(limits = c(0, 40)) +
  scale_y_continuous(limits = c(0, 80)) +
  geom_vline(xintercept=mean_contacts_site_d2$mean[2], color="black") +
  annotate("text", x=mean_contacts_site_d2$mean[2]+1, y=65,
           label=paste0(mean_contacts_site_d2$mean[2],
                        " (95% CI ", ci_contacts_urban_d2$ll,"-",
                        ci_contacts_urban_d2$ul,")"),
           hjust=0, size=4) +
  labs(x="",
       y="Freq.") +
  theme_classic() +
  axis_text_theme2
# hist_urban

contact_hist_d2 <- contact_hist_rural_d2 / contact_hist_urban_d2

# ggsave(contact_hist_d2, filename = "../../output/figs/fig_contact_hist_d2.pdf",
#        height=6, width=8, dpi=300,
#        bg="#FFFFFF")

# contact_hist_d2


#| label: fig-contact-boxplot-age
# median by attributes
# 1. age
contacts_age <- df_contact_d1 %>%
  dplyr::group_by(rec_id, study_site, participant_age) %>%   # group by id and count number of df_contact_d1
  dplyr::summarize(num_contacts = n())

fig_contacts_age_box <- ggplot(contacts_age, aes(x = participant_age, 
                                                 y = num_contacts, 
                                                 fill = study_site)) +
  geom_boxplot(width = 0.4, position = position_dodge(width = 0.5), outlier.shape = NA) +
  scale_fill_manual(values = cols_site) +
  labs(x = "Participant age", y = "# of contacts") +
  ylim(0, 30) +
  theme_classic() +
  axis_text_theme2 +
  theme(legend.position = "none") 
# theme(legend.key.size = unit(1, "lines"),
#       legend.direction = "horizontal")

# ggsave(fig_contacts_age_box, filename = "../output/figs/fig_contact_age_box.pdf",
#        height=6, width=8, dpi=300,
#        bg="#FFFFFF")

fig_contacts_age_box


#| label: fig-baseline-distributions
#| include: false

fig_baseline_distributions <- wrap_plots(fig_sex_age, contact_hist_d1, fig_contacts_age_box) + 
  plot_annotation(tag_levels = 'A') + theme(plot.tag = element_text(size = 12)) +
  plot_layout(nrow=3, heights = c(400, 200, 200))

# fig_baseline_distributions


#| label: fig-contact-boxplot-sex
contacts_sex <- df_contact_d1 %>%
  # group by id and count number of contacts
  dplyr::group_by(rec_id, study_site, participant_age, participant_sex) %>%   
  dplyr::summarize(num_contacts = n())
# mean_contacts_sex <- 

contacts_male <- contacts_sex %>% filter(participant_sex == "Male")
contacts_female <- contacts_sex %>% filter(participant_sex == "Female")
mean_contacts_male <- mean(contacts_male$num_contacts)
mean_contacts_female <- mean(contacts_female$num_contacts)
wilcoxtest_bysex <- round(wilcox.test(contacts_male$num_contacts, 
                                      contacts_female$num_contacts)$p.value,2)
# wilcoxtest_bysex # 0.1

# sex by site
contacts_male_rural <- contacts_male %>% filter(study_site == "Rural")
contacts_female_rural <- contacts_female %>% filter(study_site == "Rural")
mean_contacts_male_rural <- mean(contacts_male_rural$num_contacts)
mean_contacts_female_rural <- mean(contacts_female_rural$num_contacts)
wilcoxtest_rural_bysex <- round(wilcox.test(contacts_male_rural$num_contacts,
                                            contacts_female_rural$num_contacts)$p.value,2)
# wilcoxtest_rural_bysex # p=0.11

contacts_male_urban <- contacts_male %>% filter(study_site == "Urban")
contacts_female_urban <- contacts_female %>% filter(study_site == "Urban")
mean_contacts_male_urban <- mean(contacts_male_urban$num_contacts)
mean_contacts_female_urban <- mean(contacts_female_urban$num_contacts)
wilcoxtest_urban_bysex <- round(wilcox.test(contacts_male_urban$num_contacts,
                                            contacts_female_urban$num_contacts)$p.value,2) # p=0.35
wilcoxtest_urban_bysex # p=0.34

fig_sex_box <- fxn_fig_boxplot(contacts_sex %>% 
                                 filter(!is.na(participant_sex)), 
                               "participant_sex") +
  axis_text_theme2
# fig_sex_box

# ggsave(fig_sex_box, filename = "../../output/figs/moz_sex_site_boxplot.pdf",
#        height=6, width=8, dpi=300,
#        bg="#FFFFFF")

# fig_sex_box



#| include: false
contacts_type <- df_contact_d1 %>%
  filter(touch_contact != "I don't remember") %>%
  dplyr::group_by(rec_id, study_site, participant_age, touch_contact) %>%
  dplyr::summarize(num_contacts = n())

# Ho: there is no difference in mean number of contacts between touch and physical        
contacts_touch_rural <- contacts_type %>% filter(touch_contact == "Yes") %>% filter(study_site == "Rural")
contacts_conv_rural <- contacts_type %>% filter(touch_contact == "No") %>% filter(study_site == "Rural")
contacts_touch_urban <- contacts_type %>% filter(touch_contact == "Yes") %>% filter(study_site == "Urban")
contacts_conv_urban <- contacts_type %>% filter(touch_contact == "No")  %>% filter(study_site == "Urban")

mean_contacts_touch_rural <- round(mean(contacts_touch_rural$num_contacts), 1)
mean_contacts_conv_rural <- round(mean(contacts_conv_rural$num_contacts), 1)
mean_contacts_touch_urban <- round(mean(contacts_touch_urban$num_contacts), 1)
mean_contacts_conv_urban <- round(mean(contacts_conv_urban$num_contacts), 1)

wilcoxtest_bytouch_rural <- round(wilcox.test(contacts_touch_rural$num_contacts, 
                                              contacts_conv_rural$num_contacts)$p.value, 10) 
wilcoxtest_bytouch_rural# p<0.05

wilcoxtest_bytouch_urban <- round(wilcox.test(contacts_touch_urban$num_contacts, 
                                              contacts_conv_urban$num_contacts)$p.value, 2) # p<0.05
wilcoxtest_bytouch_urban

fig_touch_box <- fxn_fig_boxplot(contacts_type, "touch_contact") +
  axis_text_theme2
fig_touch_box

# ggsave(fig_touch_box, filename = "../../output/figs/fig_contact_touch_box.pdf",
#        height=6, width=8, dpi=300,
#        bg="#FFFFFF")

# fig_touch_box


#| label: fig-contact-boxplot-hhmembership
# overall
contacts_hhmember <- df_contact_d1 %>%
  dplyr::group_by(rec_id, study_site, participant_age, hh_membership) %>% 
  dplyr::summarize(num_contacts = n())

fig_hhmembership_box <- fxn_fig_boxplot(contacts_hhmember, "hh_membership") +
  axis_text_theme2 +
  theme(legend.position = c(0.3, 0.9))
fig_hhmembership_box
# ggsave(fig_hhmembership_box, filename = "../output/figs/moz_hh_membership_boxplot.pdf",
#        height=6, width=8, dpi=300,
#        bg="#FFFFFF")

# Ho: there is no difference in number of contacts between participants and members vs non-members        
hhmember <- contacts_hhmember %>% filter(hh_membership == "Member")
nonmember <- contacts_hhmember %>% filter(hh_membership == "Non-member")
mean_contacts_hhmember <- mean(hhmember$num_contacts)
mean_contacts_nonmember <- mean(nonmember$num_contacts)
wilcoxtest_byhhmembership <- round(wilcox.test(hhmember$num_contacts, 
                                               nonmember$num_contacts)$p.value,2) # p<0.05
wilcoxtest_byhhmembership


contacts_hhmember_rural <- hhmember %>% filter(study_site == "Rural")
contacts_nonmember_rural <- nonmember %>% filter(study_site == "Rural")
contacts_hhmember_urban <- hhmember %>% filter(study_site == "Urban")
contacts_nonmember_urban <- nonmember  %>% filter(study_site == "Urban")

mean_contacts_hhmember_rural <- mean(contacts_hhmember_rural$num_contacts, na.rm=T)
mean_contacts_nonmember_rural <- mean(contacts_nonmember_rural$num_contacts)
mean_contacts_hhmember_urban <- mean(contacts_hhmember_urban$num_contacts, na.rm=T)
mean_contacts_nonmember_urban <- mean(contacts_nonmember_urban$num_contacts %>% na.omit())

wilcoxtest_hhmember_rural <- round(wilcox.test(contacts_hhmember_rural$num_contacts, 
                                               contacts_nonmember_rural$num_contacts)$p.value, 2) 
# wilcoxtest_hhmember_rural # p<0.05

wilcoxtest_hhmember_urban <- round(wilcox.test(contacts_hhmember_urban$num_contacts, 
                                               contacts_nonmember_urban$num_contacts)$p.value, 2) # p<0.05
# wilcoxtest_hhmember_urban # p<0.05

rm(hhmember, nonmember) 
# fig_hhmembership_box


#| label: fig-contact-boxplot-education

# enrolled in school
contacts_education <- df_contact_d1 %>%
  # group by id and count number of contacts
  dplyr::group_by(rec_id, study_site, participant_age, enrolled_school) %>%   
  drop_na(enrolled_school) %>%
  dplyr::summarize(num_contacts = n())

# Ho: there is no difference in number of contacts between those in school vs not in school       
rural_inschool <- contacts_education %>% filter(enrolled_school == "Yes" & study_site == "Rural")
rural_notinschool <- contacts_education %>% filter(enrolled_school == "No" & study_site == "Rural")
ci_contacts_rural_inschool_d1 <- lm(num_contacts ~ 1, rural_inschool)
mean_contacts_rural_inschool_d1 <- ci_contacts_rural_inschool_d1$coefficients
ci_contacts_rural_inschool_d1  <- as.data.frame(round(confint(ci_contacts_rural_inschool_d1),1))
names(ci_contacts_rural_inschool_d1)[1] <- "ll"
names(ci_contacts_rural_inschool_d1)[2] <- "ul"

ci_contacts_rural_notinschool_d1 <- lm(num_contacts ~ 1, rural_notinschool)
mean_contacts_rural_notinschool_d1 <- ci_contacts_rural_notinschool_d1$coefficients
ci_contacts_rural_notinschool_d1  <- as.data.frame(round(confint(ci_contacts_rural_notinschool_d1),1))
names(ci_contacts_rural_notinschool_d1)[1] <- "ll"
names(ci_contacts_rural_notinschool_d1)[2] <- "ul"

wilcoxtest_rural_byschool <- round(wilcox.test(rural_inschool$num_contacts, 
                                               rural_notinschool$num_contacts)$p.value,2) # p<0.05
wilcoxtest_rural_byschool


urban_inschool <- contacts_education %>% filter(enrolled_school == "Yes" & study_site == "Urban")
urban_notinschool <- contacts_education %>% filter(enrolled_school == "No" & study_site == "Urban")
ci_contacts_urban_inschool_d1 <- lm(num_contacts ~ 1, urban_inschool)
mean_contacts_urban_inschool_d1 <- ci_contacts_urban_inschool_d1$coefficients
ci_contacts_urban_inschool_d1  <- as.data.frame(round(confint(ci_contacts_urban_inschool_d1),1))
names(ci_contacts_urban_inschool_d1)[1] <- "ll"
names(ci_contacts_urban_inschool_d1)[2] <- "ul"

ci_contacts_urban_notinschool_d1 <- lm(num_contacts ~ 1, urban_notinschool)
mean_contacts_urban_notinschool <- ci_contacts_urban_notinschool_d1$coefficients
ci_contacts_urban_notinschool_d1  <- as.data.frame(round(confint(ci_contacts_urban_notinschool_d1),1))
names(ci_contacts_urban_notinschool_d1)[1] <- "ll"
names(ci_contacts_urban_notinschool_d1)[2] <- "ul"

wilcoxtest_urban_byschool <- round(wilcox.test(urban_inschool$num_contacts, 
                                               urban_notinschool$num_contacts)$p.value,2) # p<0.05
wilcoxtest_urban_byschool


fig_education_box <- fxn_fig_boxplot(contacts_education, "enrolled_school") +
  axis_text_theme2

# ggsave(fig_education_box, filename = "../output/figs/fig_education_box.pdf",
#        height=6, width=8, dpi=300,
#        bg="#FFFFFF")

# fig_education_box



#| label: fig-contact-boxplot-dayweek
# day of week
contacts_dayweek <- df_contact_d1 %>%
  dplyr::group_by(rec_id, study_site, participant_age, participant_sex, day_of_week) %>%
  dplyr::summarize(num_contacts = n()) %>%
  na.omit()

fig_dayweek_box <- contacts_dayweek %>%
  ggplot(aes(x = participant_age, y = num_contacts, fill = day_of_week)) +
  geom_boxplot(width = 0.4, position = position_dodge(width = 0.5)) +
  facet_wrap(~study_site, nrow = 2) +
  scale_fill_manual(values = c("#a6bddb", "#67a9cf", "#3690c0", 
                               "#02818a", "#02818a", "#fed976", "#fd8d3c")) +
  labs(x = "Participant age", y = "Number of contacts") +
  ylim(0, 40) +
  axis_text_theme2  +
  theme(legend.position = c(0.4, 0.9))

# ggsave(fig_dayweek_box, filename = "../output/figs/fig_dayweek_box.pdf",
#        height=6, width=8, dpi=300,
#        bg="#FFFFFF")

fig_dayweek_box



#| label: fig-contact-boxplot-weekday

# weekday/weekend
contacts_weekday <- df_contact_d1 %>%
  dplyr::group_by(rec_id, study_site, participant_age, weekday) %>%
  dplyr::summarize(num_contacts = n()) %>%
  na.omit()

# Ho: there is no difference in number of contacts between those in school vs not in school       
contacts_rural_weekday <- contacts_weekday %>% filter(weekday == "Weekday" & study_site == "Rural")
contacts_rural_weekend <- contacts_weekday %>% filter(weekday == "Weekend" & study_site == "Rural")

# Ho: there is no difference in number of contacts between weekday vs weekend  in rural     
ci_contacts_rural_weekday_d1 <- lm(num_contacts ~ 1, contacts_rural_weekday)
mean_contacts_rural_weekday_d1 <- ci_contacts_rural_weekday_d1$coefficients
ci_contacts_rural_weekday_d1  <- as.data.frame(round(confint(ci_contacts_rural_weekday_d1),1))
names(ci_contacts_rural_weekday_d1)[1] <- "ll"
names(ci_contacts_rural_weekday_d1)[2] <- "ul"
mean_contacts_rural_weekday_d1 
# ci_contacts_rural_weekday_d1

ci_contacts_rural_weekend_d1 <- lm(num_contacts ~ 1, contacts_rural_weekend)
mean_contacts_rural_weekend_d1 <- ci_contacts_rural_weekend_d1$coefficients
ci_contacts_rural_weekend_d1  <- as.data.frame(round(confint(ci_contacts_rural_weekend_d1),1))
names(ci_contacts_rural_weekend_d1)[1] <- "ll"
names(ci_contacts_rural_weekend_d1)[2] <- "ul"
mean_contacts_rural_weekend_d1

wilcoxtest_rural_weekday <- round(wilcox.test(contacts_rural_weekday$num_contacts, 
                                              contacts_rural_weekend$num_contacts)$p.value,2) # p<0.05
wilcoxtest_rural_weekday


# Ho: there is no difference in number of contacts between weekday vs weekend      
contacts_urban_weekday <- contacts_weekday %>% filter(weekday == "Weekday" & study_site == "Urban")
contacts_urban_weekend <- contacts_weekday %>% filter(weekday == "Weekend" & study_site == "Urban")

ci_contacts_urban_weekday_d1 <- lm(num_contacts ~ 1, contacts_urban_weekday)
mean_contacts_urban_weekday_d1 <- ci_contacts_urban_weekday_d1$coefficients
ci_contacts_urban_weekday_d1  <- as.data.frame(round(confint(ci_contacts_urban_weekday_d1),1))
names(ci_contacts_urban_weekday_d1)[1] <- "ll"
names(ci_contacts_urban_weekday_d1)[2] <- "ul"
mean_contacts_urban_weekday_d1 
# ci_contacts_urban_weekday_d1

ci_contacts_urban_weekend_d1 <- lm(num_contacts ~ 1, contacts_urban_weekend)
mean_contacts_urban_weekend_d1 <- ci_contacts_urban_weekend_d1$coefficients
ci_contacts_urban_weekend_d1  <- as.data.frame(round(confint(ci_contacts_urban_weekend_d1),1))
names(ci_contacts_urban_weekend_d1)[1] <- "ll"
names(ci_contacts_urban_weekend_d1)[2] <- "ul"
mean_contacts_urban_weekend_d1

wilcoxtest_urban_weekday <- round(wilcox.test(contacts_urban_weekday$num_contacts, 
                                              contacts_urban_weekend$num_contacts)$p.value,2) # p<0.05
wilcoxtest_urban_weekday

fig_weekday_box <- fxn_fig_boxplot(contacts_weekday, "weekday") +
  axis_text_theme2  +
  theme(legend.position = c(0.3, 0.9))

# ggsave(fig_weekday_box, filename = "../../output/figs/fig_weekday_box.pdf",
#        height=6, width=8, dpi=300,
#        bg="#FFFFFF")


# fig7_weekday_rural_box <- contacts_weekday %>%
#   dplyr::filter(study_site == "Rural") %>%
#   plotly::plot_ly(x=~participant_age,  y=~num_contacts,  color=~weekday,  
#                   type = 'box',  colors = c("#ef8a62", "#67a9cf")) %>%
#   plotly::layout(#annotations = title_rural_d1,
#     xaxis = list(title= 'Participant age'),  
#     yaxis = list(title='Number of contacts',
#                  range=list(0,40))) %>%
#   plotly::layout(boxmode = "group")
# 
# fig7_weekday_urban_box <- contacts_weekday %>%
#   dplyr::filter(study_site == "Urban") %>%
#   plotly::plot_ly(x=~participant_age,  y=~num_contacts,  color=~weekday,  
#                   type = 'box',  colors = c("#ef8a62", "#67a9cf")) %>%
#   plotly::layout(#annotations = title_urban_d1,
#     xaxis = list(title= 'Participant age'),  
#     yaxis = list(title='Number of contacts',
#                  range=list(0,40))) %>%
#   plotly::layout(boxmode = "group")
# 
# subplot(style(fig7_weekday_rural_box, showlegend = F), nrows=2, fig7_weekday_urban_box, 
#         shareX = TRUE, margin = 0.05)




#| label: fig-contact-boxplot-weekyear
# week numbers for 2021: https://www.epochconverter.com/weeks/2021

# week of the year
# week of the year
contacts_weekyear <- df_contact_d1 %>%
  dplyr::group_by(rec_id, study_site, participant_age, weekyear) %>%
  dplyr::summarize(num_contacts = n(),
                   n_participants = length(unique(rec_id)))

# number of unique participants per week
participants_weekyear <- df_contact_d1 %>%
  dplyr::group_by(study_site, weekyear) %>%
  dplyr::summarize(n_participants = length(unique(rec_id)))

fig_weekyear_rural_box <- contacts_weekyear %>%
  dplyr::filter(study_site == "Rural") %>%
  plotly::plot_ly(x=~weekyear,  y=~num_contacts,  color=~study_site,  
                  type = 'box',  colors = c("#ef8a62", "#67a9cf")) %>%
  plotly::layout(#annotations = title_rural_d1,
    xaxis = list(title= 'Week of year'),  
    yaxis = list(title='Number of contacts',
                 range=list(0,40))) %>%
  plotly::layout(boxmode = "group") %>%
  
  # highlight COVID-19 periods: refs
  # https://allafrica.com/stories/202106281037.html
  # https://www.science.org/doi/10.1126/science.abq5358
  plotly::layout(shapes = list(
    # wave 2, Oct 2020-Feb 2021. restrictions eased in May
    
    # wave 3, delta, Apr 26 2021 - Aug 2 2021
    list(type = "rect",
         fillcolor = "#bdbdbd", line=list(color="#f0f0f0"), opacity=0.3,
         x0="5", x1="18", xref="x",
         y0=0, y1=40, yref="y"),
    
    # wave 4. omicron, Oct 2021-Jan 2022
    list(type = "rect",
         fillcolor = "#bdbdbd", line=list(color="#f0f0f0"), opacity=0.3,
         x0="29", x1="2022-2", xref="x",
         y0=0, y1=40, yref="y")
  )) %>%
  add_annotations(text = "Wave 3, Delta",
                  x=0.2, y=0.85,
                  size = 12,
                  xref = "paper", yref = "paper",  
                  xanchor = "center", yanchor = "bottom",  
                  showarrow = FALSE) %>%
  add_annotations(text = "Wave 4, Omicron",
                  size = 12,
                  x=0.66, y=0.85,
                  xref = "paper", yref = "paper",  
                  xanchor = "center", yanchor = "bottom",  
                  showarrow = FALSE)
# fig8_weekyear_rural_box

fig_weekyear_urban_box <- contacts_weekyear %>%
  dplyr::filter(study_site == "Urban") %>%
  plotly::plot_ly(x=~weekyear,  y=~num_contacts,  color=~study_site,  
                  type = 'box',  colors = c("#ef8a62", "#67a9cf")) %>%
  plotly::layout(#annotations = title_urban_d1,
    xaxis = list(title= 'Week of year'),  
    yaxis = list(title='Number of contacts',
                 range=list(0,40))) %>%
  plotly::layout(boxmode = "group")  %>%
  
  plotly::layout(shapes = list(
    # wave 2, Oct 2020-Feb 2021. restrictions eased in May
    
    # wave 3, delta, Apr 26 2021 - Aug 2 2021
    list(type = "rect",
         fillcolor = "#bdbdbd", line=list(color="#f0f0f0"), opacity=0.3,
         x0="5", x1="18", xref="x",
         y0=0, y1=40, yref="y"),
    
    # wave 4. omicron, Oct 2021-Jan 2022
    list(type = "rect",
         fillcolor = "#bdbdbd", line=list(color="#f0f0f0"), opacity=0.3,
         x0="29", x1="2022-2", xref="x",
         y0=0, y1=40, yref="y")
  )) %>%
  add_annotations(text = "Wave 3, Delta",
                  x=0.2, y=0.85,
                  size = 12,
                  xref = "paper", yref = "paper",  
                  xanchor = "center", yanchor = "bottom",  
                  showarrow = FALSE) %>%
  add_annotations(text = "Wave 4, Omicron",
                  size = 12,
                  x=0.66, y=0.85,
                  xref = "paper", yref = "paper",  
                  xanchor = "center", yanchor = "bottom",  
                  showarrow = FALSE) 

# participants_weekyear %>%
#   dplyr::filter(study_site == "Urban") %>%
#   plotly::plot_ly(x=~weekyear,  y=~n_participants,  
#                       type = 'scatter', mode="line") %>%
#   plotly::layout(annotations = title_urban_d1,
#                  xaxis = list(title= 'Week of year'),  
#                  yaxis = list(title='Number of participants',
#                               range=list(0,50)))

fig_weekyear_site_box <- subplot(style(fig_weekyear_rural_box, showlegend = F), nrows=2, 
                                 fig_weekyear_urban_box, 
                                 shareX = TRUE, margin = 0.05)

# ggsave(fig_weekyear_site_box, filename = "../../output/figs/fig_weekyear_site_box.pdf",
#        height=6, width=8, dpi=300,
#        bg="#FFFFFF")

fig_weekyear_site_box


#| label: fig-contact-boxplot-arisymptom
table(participants$ari_symptom) # 
# overall
contacts_ari <- df_contact_d1 %>%
  dplyr::group_by(rec_id, study_site, participant_age, ari_symptom) %>%   # group by id and count number of contacts
  dplyr::summarize(num_contacts = n())

# Ho: there is no difference in number of contacts between those with ARI vs no-ARI symptoms      
contacts_rural_ari <- contacts_ari %>% filter(ari_symptom == ">1 symptom" & study_site == "Rural")
contacts_rural_noari <- contacts_ari %>% filter(ari_symptom == "No symptom" & study_site == "Rural")

ci_contacts_rural_ari_d1 <- lm(num_contacts ~ 1, contacts_rural_ari)
mean_contacts_rural_ari_d1 <- ci_contacts_rural_ari_d1$coefficients
ci_contacts_rural_ari_d1  <- as.data.frame(round(confint(ci_contacts_rural_ari_d1),1))
names(ci_contacts_rural_ari_d1)[1] <- "ll"
names(ci_contacts_rural_ari_d1)[2] <- "ul"
mean_contacts_rural_ari_d1 

ci_contacts_rural_noari_d1 <- lm(num_contacts ~ 1, contacts_rural_noari)
mean_contacts_rural_noari_d1 <- ci_contacts_rural_noari_d1$coefficients
ci_contacts_rural_noari_d1  <- as.data.frame(round(confint(ci_contacts_rural_noari_d1),1))
names(ci_contacts_rural_noari_d1)[1] <- "ll"
names(ci_contacts_rural_noari_d1)[2] <- "ul"
mean_contacts_rural_noari_d1 

wilcoxtest_rural_byari <- wilcox.test(contacts_rural_ari$num_contacts, 
                                      contacts_rural_noari$num_contacts)
wilcoxtest_rural_byari # p=0.06


# Ho: there is no difference in number of contacts between those with ARI vs no-ARI symptoms      
contacts_urban_ari <- contacts_ari %>% filter(ari_symptom == ">1 symptom" & study_site == "Urban")
contacts_urban_noari <- contacts_ari %>% filter(ari_symptom == "No symptom" & study_site == "Urban")

ci_contacts_urban_ari_d1 <- lm(num_contacts ~ 1, contacts_urban_ari)
mean_contacts_urban_ari_d1 <- ci_contacts_urban_ari_d1$coefficients
ci_contacts_urban_ari_d1  <- as.data.frame(round(confint(ci_contacts_urban_ari_d1),1))
names(ci_contacts_urban_ari_d1)[1] <- "ll"
names(ci_contacts_urban_ari_d1)[2] <- "ul"
mean_contacts_urban_ari_d1 

ci_contacts_urban_noari_d1 <- lm(num_contacts ~ 1, contacts_urban_noari)
mean_contacts_urban_noari_d1 <- ci_contacts_urban_noari_d1$coefficients
ci_contacts_urban_noari_d1  <- as.data.frame(round(confint(ci_contacts_urban_noari_d1),1))
names(ci_contacts_urban_noari_d1)[1] <- "ll"
names(ci_contacts_urban_noari_d1)[2] <- "ul"
mean_contacts_urban_noari_d1 

wilcoxtest_urban_byari <- wilcox.test(contacts_urban_ari$num_contacts, 
                                      contacts_urban_noari$num_contacts)
wilcoxtest_urban_byari # p=0.67


fig_ari_box <- fxn_fig_boxplot(contacts_ari, "ari_symptom") +
  axis_text_theme2 +
  theme(legend.position = c(0.3, 0.9))

# ggsave(fig_ari_box, filename = "../output/figs/fig_ARI_box.pdf",
#        height=6, width=8, dpi=300,
#        bg="#FFFFFF")
#        
fig_ari_box


#| label: fig-contact-boxplot-agesymptom
# table(participants$age_symptom) # 24/1207 with AGE symptoms

# overall
contacts_age <- df_contact_d1 %>%
  # group by id and count number of contacts
  dplyr::group_by(rec_id, study_site, participant_age, age_symptom) %>%   
  dplyr::summarize(num_contacts = n())

# Ho: there is no difference in number of contacts between those with AGE vs no-AGE symptoms      
age <- contacts_age %>% filter(age_symptom == "Yes")
noage <- contacts_age %>% filter(age_symptom == "No")
mean_contacts_age <- mean(age$num_contacts)
mean_contacts_noage <- mean(noage$num_contacts)
wilcoxtest_byage <- wilcox.test(age$num_contacts, noage$num_contacts) # p=0.24

fig_AGE_box <- fxn_fig_boxplot(contacts_age, "age_symptom")  +
  axis_text_theme2
# fig_AGE_box


# ==
# crude matrices, non-symmetrical

# this only assumes contacts occur within the same age groups, hence the paired ages
standard_str <- data.frame(participant_age = rep(unique(df_contact$participant_age), each=10),
                           contact_age = rep(unique(df_contact$participant_age),each = 10))

## rural site 
n_participants_rural <- participants %>%
  dplyr::filter(study_site == "Rural") %>%
  dplyr::group_by(participant_age) %>%
  summarize(n_participants = n())

m1_rural_contact_table <- df_contact_d1 %>%
  dplyr::filter(study_site == "Rural")
table(m1_rural_contact_table$participant_age, m1_rural_contact_table$contact_age)

m1_rural <- df_contact_d1 %>%
  dplyr::filter(study_site == "Rural") %>%
  dplyr::group_by(participant_age, contact_age) %>%
  summarize(total_contacts = n()) %>%
  full_join(standard_str, by = c("participant_age", "contact_age"), keep = F) %>%
  dplyr::mutate(total_contacts = replace_na(total_contacts, 0)) %>% # if total_contacts==NA, replace with 0
  drop_na() %>%
  distinct(participant_age, contact_age, .keep_all = T) %>%
  left_join(n_participants_rural, by="participant_age")  %>%
  dplyr::mutate(average_contact = round(total_contacts / n_participants_rural$n_participants, digits = 1))

## urban site
n_participants_urban <- participants %>%
  dplyr::filter(study_site == "Urban") %>%
  dplyr::group_by(participant_age) %>%
  summarize(n_participants = n())
# participants 15-19y did not have contacts with <6mo, so we generate that row
missing_data_urban <- data.frame(participant_age="15-19y", contact_age="<6mo", 
                                 total_contacts=0, n_participants=60)

m1_urban_contact_table <- df_contact_d1 %>%
  dplyr::filter(study_site == "Urban")
table(m1_urban_contact_table$participant_age, m1_urban_contact_table$contact_age)

m1_urban <- df_contact_d1 %>%
  dplyr::filter(study_site == "Urban") %>%
  mutate(participant_age = as.character(participant_age),
         contact_age = as.character(contact_age)) %>%
  dplyr::group_by(participant_age, contact_age) %>%
  dplyr::summarize(total_contacts = n()) %>%
  drop_na(contact_age) %>%
  left_join(n_participants_urban, by="participant_age") %>%
  rbind(missing_data_urban) %>%
  mutate(average_contact = (round(total_contacts/n_participants_urban$n_participants, 1))) %>%
  # rename(average_contact = "average_contact$n") %>%
  dplyr::select(participant_age, contact_age, average_contact) %>%
  dplyr::mutate(participant_age = factor(participant_age,
                                         levels = c("<6mo", "6-11mo", "1-4y","5-9y", 
                                                    "10-14y", "15-19y", "20-29y", 
                                                    "30-39y", "40-59y", "60+y")),
                contact_age = factor(contact_age,
                                     levels = c("<6mo", "6-11mo", "1-4y","5-9y", 
                                                "10-14y", "15-19y", "20-29y", 
                                                "30-39y", "40-59y", "60+y")))

### figures

rural_matrix <- fun_matrix1_plot(m1_rural, "Rural", xlab = "Participant age", ylab = "Contact age") 
matrix_legend <- get_legend(rural_matrix)
rural_matrix <- rural_matrix + theme(legend.position = "none")

urban_matrix <- fun_matrix1_plot(m1_urban, "Urban", xlab = "Participant age", ylab = "") +
  theme(axis.text.y = element_blank())

#### save graph
# fig_crude_matrix <- rural_matrix | urban_matrix

fig_crude_matrix <- subplot(style(rural_matrix, showlegend = F),
                             urban_matrix, shareY = TRUE) %>%
  layout(title = '',
         # xaxis = list(title = "Participant age"),
         annotations = list(
           list(x=0.05, y=1.0,
                text = "A. Rural",
                xref = "paper",
                yref = "paper",
                xanchor = "center",
                yanchor = "bottom",
                showarrow = FALSE),
           list(x=0.57, y=1.0,
                text = "B. Urban",
                xref = "paper",
                yref = "paper",
                xanchor = "center",
                yanchor = "bottom",
                showarrow = FALSE),
           list(x=0.5, y=-0.3,
                text = "Participant age",
                xref = "paper",
                yref = "paper",
                xanchor = "center",
                yanchor = "bottom",
                showarrow = FALSE,
                font = list(size = 18, face = "bold")
           )),
         height = 400)
# fig_crude_matrix2
orca(fig_crude_matrix, "../../output/figs/fig_contacts_crude_matrix.pdf")
# rm(fig_crude_matrix2)


# ===
# generate, draw and display symmetric matrix plot
rural_symmetric_matrix <- fun_symmetric_matrix(df_contact_d1, "Rural", n_participants_rural)
rural_symmetric_matrix_plot <- fun_symmetric_plot(rural_symmetric_matrix, "Rural","Participant age","Contact age")
rural_symmetric_matrix_plot
matrix_legend <- get_legend(rural_symmetric_matrix_plot)
rural_symmetric_matrix_plot <- rural_symmetric_matrix_plot + 
  theme(legend.position = "none")

urban_symmetric_matrix <- fun_symmetric_matrix(df_contact_d1, "Urban", n_participants_urban)
urban_symmetric_matrix_plot <- fun_symmetric_plot(urban_symmetric_matrix, "Urban","Participant age","")
urban_symmetric_matrix_plot

rural_symmetric_matrix_plot | urban_symmetric_matrix_plot

wrap_plots(rural_symmetric_matrix_plot, urban_symmetric_matrix_plot) + 
  plot_annotation(tag_levels = 'A') + 
  theme(plot.tag = element_text(size = 12)) +
  plot_layout(nrow=1, heights = c(600))


# fig_sex_age, removed
fig_baseline_distributions <- wrap_plots(contact_hist_d1, 
                                         fig_contacts_age_box, 
                                         fig_crude_matrix) + 
  plot_annotation(tag_levels = 'A') + 
  theme(plot.tag = element_text(size = 12)) +
  plot_layout(nrow=3, heights = c(600, 600, 600))

fig_baseline_distributions

ggsave(fig_baseline_distributions, filename = "../../output/figs/fig_baseline_distributions_v2.pdf",
       height=10, width=8, dpi=300,
       bg="#FFFFFF")


#| label: fig-matrix-type

# n_participants_rural_touch <- participants %>%
#   dplyr::filter(study_site == "Rural") %>%
#   dplyr::group_by(participant_age) %>%
#   summarize(n_participants = n())

m1_rural_touch <- df_contact_d1 %>%
  dplyr::filter(study_site == "Rural") %>%
  dplyr::filter(touch_contact == "Yes") %>%
  dplyr::group_by(participant_age, contact_age) %>%
  summarize(total_contacts = n()) %>%
  full_join(standard_str, by = c("participant_age", "contact_age"), keep = F) %>%
  dplyr::mutate(total_contacts = replace_na(total_contacts, 0)) %>% # if total_contacts==NA, replace with 0
  drop_na() %>%
  distinct(participant_age, contact_age, .keep_all = T) %>%
  left_join(n_participants_rural, by="participant_age")  %>%
  dplyr::mutate(average_contact = round(total_contacts / n_participants_rural$n_participants, digits = 1))

missing_data_rural_conv <- data.frame(participant_age = c("<6mo", "1-4y", "10-14y", "15-19y", "20-29y",
                                                          "30-39y", "5-9y", "6-11mo", "<6mo", "10-14y",
                                                          "20-29y", "30-39y", "40-59y", "6-11mo", "<6mo"),
                                      contact_age = c("<6mo","<6mo","<6mo","<6mo", "<6mo", "<6mo", "<6mo",
                                                      "<6mo", "6-11mo", "6-11mo", "6-11mo", "6-11mo",
                                                      "6-11mo", "6-11mo", "60+y"),
                                      total_contacts = c(rep(0, each=15))) %>%
  left_join(n_participants_rural, by = "participant_age")

m1_rural_conv <- df_contact_d1 %>%
  dplyr::filter(study_site == "Rural") %>%
  dplyr::filter(touch_contact == "No") %>%
  mutate(participant_age = as.character(participant_age),
         contact_age = as.character(contact_age)) %>%
  dplyr::group_by(participant_age, contact_age) %>%
  dplyr::summarize(total_contacts = n()) %>%
  drop_na(contact_age) %>%
  left_join(n_participants_rural, by="participant_age") %>%
  rbind(missing_data_rural_conv) %>%
  mutate(average_contact = (round(total_contacts/n_participants_rural$n_participants, 1))) %>%
  # rename(average_contact = "average_contact$n") %>%
  dplyr::select(participant_age, contact_age, average_contact) %>%
  dplyr::mutate(participant_age = factor(participant_age,
                                         levels = c("<6mo", "6-11mo", "1-4y","5-9y", 
                                                    "10-14y", "15-19y", "20-29y", 
                                                    "30-39y", "40-59y", "60+y")),
                contact_age = factor(contact_age,
                                     levels = c("<6mo", "6-11mo", "1-4y","5-9y", 
                                                "10-14y", "15-19y", "20-29y", 
                                                "30-39y", "40-59y", "60+y")))

rural_matrix_touch <- fun_matrix2_plot(m1_rural_touch, "A. Rural physical", 
                                       xlab = "Participant age", ylab = "Contact age") + 
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.title.x = element_blank())

rural_matrix_conv <- fun_matrix2_plot(m1_rural_conv, "C. Rural conversation", 
                                      xlab = "Participant age", ylab = "Contact age") + 
  theme(legend.position = "none")


# urban contacts
missing_data_urban_touch <- data.frame(participant_age = "15-19y",
                                       contact_age = "<6mo",
                                       total_contacts = 0,
                                       n_participants = 60)
m1_urban_touch <- df_contact_d1 %>%
  dplyr::filter(study_site == "Urban") %>%
  dplyr::filter(touch_contact == "Yes") %>%
  mutate(participant_age = as.character(participant_age),
         contact_age = as.character(contact_age)) %>%
  dplyr::group_by(participant_age, contact_age) %>%
  dplyr::summarize(total_contacts = n()) %>%
  drop_na(contact_age) %>%
  left_join(n_participants_urban, by="participant_age") %>%
  rbind(missing_data_urban_touch) %>%
  mutate(average_contact = (round(total_contacts/n_participants_urban$n_participants, 1))) %>%
  # rename(average_contact = "average_contact$n") %>%
  dplyr::select(participant_age, contact_age, average_contact) %>%
  dplyr::mutate(participant_age = factor(participant_age,
                                         levels = c("<6mo", "6-11mo", "1-4y","5-9y", 
                                                    "10-14y", "15-19y", "20-29y", 
                                                    "30-39y", "40-59y", "60+y")),
                contact_age = factor(contact_age,
                                     levels = c("<6mo", "6-11mo", "1-4y","5-9y", 
                                                "10-14y", "15-19y", "20-29y", 
                                                "30-39y", "40-59y", "60+y")))

missing_data_urban_conv <- data.frame(participant_age = c("<6mo", "6-11mo", "1-4y", "5-9y", 
                                                          "10-14y", "15-19y", "20-29y", 
                                                          "30-39y", "40-59y", "10-14y", 
                                                          "6-11mo", "1-4y", "1-4y", 
                                                          "20-29y", "15-19y", "20-29y", 
                                                          "5-9y", "6-11mo", "60+y", 
                                                          "5-9y", "6-11mo"), 
                                      contact_age=c("<6mo", "<6mo", "<6mo", "<6mo", 
                                                    "<6mo", "<6mo", "<6mo", "<6mo",
                                                    "<6mo", "1-4y", "1-4y", "20-29y", 
                                                    "40-59y", "5-9y", "6-11mo", "6-11mo", 
                                                    "6-11mo", "6-11mo", "6-11mo", "60+y", 
                                                    "60+y"), 
                                      total_contacts=c(rep(0,each=21))) %>%
  left_join(n_participants_urban, , by = "participant_age")

m1_urban_conv <- df_contact_d1 %>%
  dplyr::filter(study_site == "Urban") %>%
  dplyr::filter(touch_contact == "No") %>%
  mutate(participant_age = as.character(participant_age),
         contact_age = as.character(contact_age)) %>%
  dplyr::group_by(participant_age, contact_age) %>%
  dplyr::summarize(total_contacts = n()) %>%
  drop_na(contact_age) %>%
  left_join(n_participants_urban, by="participant_age") %>%
  rbind(missing_data_urban_conv) %>%
  mutate(average_contact = (round(total_contacts/n_participants_urban$n_participants, 1))) %>%
  # rename(average_contact = "average_contact$n") %>%
  dplyr::select(participant_age, contact_age, average_contact) %>%
  dplyr::mutate(participant_age = factor(participant_age,
                                         levels = c("<6mo", "6-11mo", "1-4y","5-9y", 
                                                    "10-14y", "15-19y", "20-29y", 
                                                    "30-39y", "40-59y", "60+y")),
                contact_age = factor(contact_age,
                                     levels = c("<6mo", "6-11mo", "1-4y","5-9y", 
                                                "10-14y", "15-19y", "20-29y", 
                                                "30-39y", "40-59y", "60+y")))

urban_matrix_touch <- fun_matrix2_plot(m1_urban_touch, "B. Urban physical", 
                                       xlab = "Participant age", ylab = "Contact age") +
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())
# get legend
# matrix_type_legend <- get_legend(urban_matrix_touch)
# hide legend
# urban_matrix_touch <- urban_matrix_touch +
#   theme(legend.position = "none")

urban_matrix_conv <- fun_matrix2_plot(m1_urban_conv, "D. Urban conversation", 
                                      xlab = "Participant age", ylab = "Contact age") +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank())

# rural_matrix_type
rural_matrix_type <- rural_matrix_touch | urban_matrix_touch
# urban_matrix_type
urban_matrix_type <-  rural_matrix_conv | urban_matrix_conv

# generate comb ined matrix
fig_matrix_type <- rural_matrix_type / urban_matrix_type


ggsave(fig_matrix_type, filename = "../../output/figs/fig_matrix_type.pdf",
       height=8, width=8, dpi=1024,
       bg="#FFFFFF")

# fig_matrix_type

### Weighted contact matrix


# col_names <- c("60+y","40-59y","30-39y","20-29y","15-19y","10-14y","5-9y","1-4y","6-11mo","<6mo") 
# row_names <- c("60+y","40-59y","30-39y","20-29y","15-19y","10-14y","5-9y","1-4y","6-11mo","<6mo")
# 
# 
# mozambique_pop_dist_2021 <- rio::import("../data/pop_dist_ines_2021.csv") 
# 
# # need to get population of children aged 6-11 months to compute this.
# # assume <6mo are 50% of 6-11 mo
# rural_population <- mozambique_pop_dist_2021 %>%
#   select(age_cat, rural) 
# missing_age <- c("<6mo", 0.5*rural_population[1,2]) %>%
#   rename(age_cat = "X..6mo.")
# rural_population <- rural_population %>%
#   rbind(missing_age)
# rural_population$rural[rural_population$age_cat == "0"] <- 0.5*rural_population[1,2]
# rural_population$age_cat[rural_population$age_cat == "0"] <- "6-11mo"
# 
# rural_population <- rural_population %>%
#   mutate(age_cat = factor(age_cat, levels = c("<6mo", "6-11mo", "1-4y","5-9y", 
#                                              "10-14y", "15-19y", "20-29y", 
#                                              "30-39y", "40-59y", "60+y"))) %>%
#   mutate(rural = as.numeric(rural),
#            weight = 1/rural) %>%
#   select(weight)
# # unweighted matrix
# m1_rural_matrix <- matrix(m1_rural$average_contact$n_participants,10,10, byrow=F)
# # m1_rural_weighted[is.na(m1_rural_weighted)] = 0
# 
# # this generates a square weighted reciprocal matrix for ages >20 years.
# k <- 10
# m1_rural_w <- matrix(0,k,k) 
# 
# for(i in 1:k){
#   for(j in 1:k){
#     m1_rural_w[i,j] <- (m1_rural_matrix[i,j]*rural_population[i] + m1_rural_matrix[j,i]*rural_population[j]) / 
#       2*(pop_dist_ines_2021[i] + pop_dist_ines_2021[j])
#     if(is.na(m1_rural_w[i,j]) | is.infinite(m1_rural_w[i,j])) 
#       m1_rural_w[i,j] <- 0
#   }
# }
# 
# m_rural_w <- as.data.frame(m_rural_w)
# rownames(m_rural_w) <- row_names
# colnames(m_rural_w) <- col_names


#| label: contact-behavior
#| include: false


contact_behav <- df_contact_d1 %>% 
  dplyr::select(study_site, participant_age, touch_contact, where_contact, contact_mask2, duration_contact2) %>%
  mutate(where_contact = as.character(where_contact),
         touch_contact = as.character(touch_contact),
         contact_mask2 = as.character(contact_mask2),
         duration_contact2 = as.character(duration_contact2)) %>%
  pivot_longer(cols = touch_contact:duration_contact2, names_to="var", values_to="value") %>%
  dplyr::group_by(study_site, participant_age, var, value) %>%
  dplyr::filter(value != "I don't remember") %>%
  dplyr::summarize(n=n()) %>%
  na.omit() %>%
  left_join(df_contact_d1 %>% 
              dplyr::group_by(participant_age, study_site) %>%
              dplyr::summarize(tot_contacts=n()), by = c("participant_age"="participant_age", 
                                                         "study_site" = "study_site")) %>%
  mutate(prop = round(n/tot_contacts, digits=2)) %>%
  mutate(value = factor(value, 
                        levels = c("Yes", "No", 
                                   
                                   "Yes", "No", 
                                   # " Never met before", "<1 yr", "1-2 yrs", "3-5 yrs", 
                                   # " 6-10 yrs", ">10 yrs",
                                   
                                   "Indoors", "Outdoors", "Both",
                                   
                                   "<5mins","5-30mins","31mins-1hr",">1hr"),
                        
                        labels = c("Yes", "No",    
                                   
                                   "Yes", "No", 
                                   
                                   # " Never met", "<1yr", "1-2yrs ", "3-5yrs ", 
                                   # " 6-10yrs", ">10yrs",
                                   
                                   "Indoors", "Outdoors", "Both",
                                   
                                   "<5mins","5-30mins","31mins-1hr",">1hr")))

# function to plot behavior of contacts by age

unhighlighed_col_darker <- 'grey60'

contact_behav_fun <- function(df, action){
  df %>% 
    dplyr::filter(var == action) %>% 
    ggplot(x = prop, y = participant_age, fill = value) +
    geom_col(aes(x = prop, y = participant_age, fill = value), 
             position = "fill", col="white") +
    
    # plot by site and delete site title slab
    facet_wrap(~study_site) +
    theme(strip.background = element_blank(),
          strip.text.x = element_blank(), # remove facet titles
          panel.spacing = unit(2, "lines")) + # increase space between facets
    
    labs(title = "",
         x = "", 
         y = "Participant age",
         fill = element_blank()) +
    
    coord_cartesian(xlim = c(0, 1), 
                    # ylim = c(0.5, 20.5), 
                    expand = F, # removes white spaces at edge of plot
                    clip = 'off') + # allows drawing outside of panel
    
    # move x-axis title to the top and format
    scale_x_continuous(labels = function(x) format(x*100, digits=2, nsmall=0), 
                       breaks = seq(0, 1, 0.2),
                       position="top") + 
    axis_text_theme2 +
    theme(axis.text.x = element_text(angle = 0, hjust=1),
          axis.line.x = element_line(colour = "black"), 
          axis.ticks.x = element_line(colour = "black")) # +
  #       axis.text = element_text(colour = unhighlighed_col_darker),
  #       text = element_text(colour = unhighlighed_col_darker),
  #       plot.title = element_text(colour = 'black')) +
  
  # # format titles
  # theme(plot.title = element_text(size = 26, face="bold"),
  #       plot.title.position = "plot", 
  #       axis.title.y = element_text(size=22), #face="bold"),
  #       axis.title.x = element_text(size=22), # face="bold"),
  #       axis.text.y = element_text(size = 14),
  #       axis.text.x = element_text(size = 14, angle=0)) +
  # theme(legend.title = element_text(size=18),
  #       legend.text = element_text(size = 14),
  #       legend.position = "top",
  #       legend.box = "vertical")
}


#| label: fig-mask-wearing
fig_masking <- contact_behav_fun(contact_behav, "contact_mask2") +
  labs(title = "A. Contact with mask?", # Was the contact wearing a mask?
       x = "% of contacts") +
  theme(legend.text = element_text(size=8))
# fig_masking

#| label: fig-contact-type
fig_touch <- contact_behav_fun(contact_behav, "touch_contact") +
  labs(title = "B. Physical contact?", # Did you have a physical contact?
       x = "% of contacts") +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        legend.text = element_text(size=8))
# fig_touch


#| label: fig-contact-location
fig_location <- contact_behav_fun(contact_behav, "where_contact") +
  labs(title = "C. Contact location", # Where did the contact occur?
       x = "") +
  theme(legend.position = c(0.6, 0.9),
        legend.text = element_text(size=8))
fig_location

#| label: fig-contact-duration
fig_duration <- contact_behav_fun(contact_behav, "duration_contact2") +
  labs(title = "D. Contact duration", # What was the duration of the contact?
       x = "") +
  scale_fill_manual(values = c("#ccebc5", "#7bccc4", "#43a2ca", "#0868ac"))  +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        legend.position = c(0.5, 0.9),
        legend.text = element_text(size=8))
fig_duration

#| label:  fig-contact-behavior
fig_contact_behavior <- (fig_masking | fig_touch) / (fig_location | fig_duration)

ggsave(fig_contact_behavior, filename = "../../output/figs/fig_contact_behavior.pdf",
       height=8, width=8, dpi=300,
       bg="#FFFFFF")


## TABLES OF CHARACTERISTICS OF CONTACTS
 
#| label: tbl-contact-characteristics
label(contacts$contact_sex) <- "Contact sex"
label(contacts$contact_age) <- "Contact age"
label(contacts$contact_mask) <- "Was the contact wearing a mask?"
label(contacts$known_contact) <- "How long have you known this contact?"
label(contacts$duration_contact) <- "How long did the contact last?"
label(contacts$frequency_contact) <- "How many times do you have a contact with this person?"
label(contacts$where_contact) <- "Where did the contact occur?"
label(contacts$hh_membership) <- "Is this a member of your household?"
label(contacts$hh_member_relationship) <- "Relationship to household member"

# summary table for rural contacts
table2 <- contacts %>%
  dplyr::select(study_site, contact_age, contact_sex,contact_mask, known_contact, 
                hh_membership, duration_contact, frequency_contact, where_contact) %>%
  #  hh_member_relationship, 
  tbl_summary(by = study_site,
              percent = "col",
              digits = all_categorical() ~ 0,
              missing = "ifany") %>%
  add_overall() %>%
  bold_labels() %>%
  modify_header(label = "")
# modify_spanning_header(c("stat_1", "stat_2", "stat_3") ~ "**Survey round**") 
# %>% modify_caption("**Table 2: Contact distribution by covariates in Mozambique**")


#| label: tbl-average-contacts-d1
# table of contacts by day of study: both days, day 1 and day 2
results_list <- list(0)  ## empty list

## specify participant characteristic stratification for cross tabs/analysis
participant_variables <- data.frame(var=c("participant_sex","participant_age",  
                                          "occupation", "hh_size_cat", "enrolled_school", 
                                          "weekday", "ari_symptom", "age_symptom"),
                                    name=c("Sex", "Age", "Occupation", "Household size",
                                           "Enrolled in school", "Weekday/Weekend", 
                                           "ARI symptoms", "AGE symptoms")) %>% 
  mutate(var = as.character(var),
         name = as.character(name))

contact_variables <- data.frame(var=c("hh_membership","touch_contact", "where_contact",
                                      "frequency_contact", "known_contact", "contact_mask2"),
                                name=c("Household membership", "Did you touch?", "Contact location", 
                                       "Frequency of contact", "Do you know the contact?", 
                                       "Was contact wearing mask?"))  %>% 
  mutate(var = as.character(var),
         name = as.character(name))

for (i in 1:nrow(participant_variables)){
  x <- df_contact_d1[,participant_variables$var[[i]]] 
  # include another variable n as the number of participants per strata
  participant_variables$var[[i]]
  
  # Number and proportion of contacts in each strata
  
  t0 <- as.data.frame(cbind(table(x), # Total 
                            round(prop.table(table(x))*100, digits=0) # Proportion
  )) # number of participants
  
  colnames(t0)[1:2] <- c("Total","Col") 
  Tot <- rep("",5)
  
  # Rural median contacts for day 1
  t1_median <- df_contact_d1 %>%
    dplyr::filter(study_site == "Rural") %>%
    # since this is total contacts per person, keep only 1 distinct record
    distinct(rec_id, .keep_all = TRUE) %>% 
    dplyr::group_by(.dots = participant_variables$var[[i]]) %>% 
    drop_na(participant_variables$var[[i]]) %>%
    do(data.frame(t(quantile(.$num_contacts, na.rm=TRUE, probs=c(0.25,0.5,0.75))))) # Median and IQR      
  t1_median$med_contact <- as.character(paste(
    t1_median$X50., " (",t1_median$X25., "-", t1_median$X75.,")", sep=""))
  
  # Calculate mean and 95% confidence intervals for day 1
  t1_mean <- df_contact_d1 %>%
    dplyr::filter(study_site == "Rural") %>%
    distinct(rec_id, .keep_all = TRUE) %>%
    dplyr::group_by(.dots = participant_variables$var[[i]]) %>%
    drop_na(participant_variables$var[[i]]) %>%
    summarise(
      mean_contact = round(mean(num_contacts, na.rm = TRUE), 1),
      lower_ci = round(t.test(num_contacts, na.rm = TRUE)$conf.int[1], 1),
      upper_ci = round(t.test(num_contacts, na.rm = TRUE)$conf.int[2], 1),
    )
  t1_mean$mean_contact <- as.character(paste(
    t1_mean$mean_contact," (",t1_mean$lower_ci, "-", t1_mean$upper_ci,")", sep=""))
  
  # Median contacts for day 2
  t2_median <- df_contact_d1 %>%
    dplyr::filter(study_site == "Urban") %>%
    # since this is total contacts per person, keep only 1 distinct record
    distinct(rec_id, .keep_all = TRUE) %>% 
    dplyr::group_by(.dots = participant_variables$var[[i]]) %>%
    drop_na(participant_variables$var[[i]]) %>%
    # na.omit() %>%
    do(data.frame(t(quantile(.$num_contacts, na.rm=TRUE, probs=c(0.25,0.5,0.75))))) # Median and IQR      
  t2_median$med_contact <- as.character(paste(
    t2_median$X50.," (",t2_median$X25.,"-",t2_median$X75.,")",sep="")) # Formatting for export
  
  # Calculate mean and 95% confidence intervals for day 1
  t2_mean <- df_contact_d1 %>%
    dplyr::filter(study_site == "Urban") %>%
    distinct(rec_id, .keep_all = TRUE) %>%
    dplyr::group_by(.dots = participant_variables$var[[i]]) %>%
    drop_na(participant_variables$var[[i]]) %>%
    summarise(
      mean_contact = round(mean(num_contacts, na.rm = TRUE), 1),
      lower_ci = round(t.test(num_contacts, na.rm = TRUE)$conf.int[1], 1),
      upper_ci = round(t.test(num_contacts, na.rm = TRUE)$conf.int[2], 1),
    )
  t2_mean$mean_contact <- as.character(paste(
    t2_mean$mean_contact, " (",t2_mean$lower_ci, "-", t2_mean$upper_ci,")", sep=""))
  
  # Bind columns together and select relevant columns
  t0<-qpcR:::cbind.na(t0, t1_median$med_contact, t1_mean$mean_contact,
                      t2_median$med_contact, t2_mean$mean_contact)
  colnames(t0)[3:6] <- c("Rural Median", "Rural Mean", "Urban Median", "Urban Mean")

  # rename total column
  t0$Total <- paste(t0$Total," (","", t0$Col,")",sep="")
  t0 <- t0[,c(1,3,4,5,6)]
  t0[,1] <- as.character(t0[,1])
  t0[,2] <- as.character(t0[,2])
  
  t0<-rbind(Tot, t0)
  
  rownames(t0)[1] <- participant_variables$name[[i]]
  results_list[[i]] <- t0
}

# format table
res_restruct<- function(res) {
  res1 <- lapply(res, as.data.frame)
  res1 <- do.call(rbind, res1)
  return(res1)
}

table3 <- res_restruct(results_list)
# colnames(table3) <- c("Total (%)", "Median", "Mean", "Median", "Mean")

table3 <- kable(table3, digits = 0, align = "r") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))

# table3

rm(t0, t1, t2)



#| label: tbl-average-contacts-d2
# table of contacts day 2
results_list <- list(0)  ## empty list

for (i in 1:nrow(participant_variables)){
  x <- df_contact_d2[,participant_variables$var[[i]]] 
  # include another variable n as the number of participants per strata
  participant_variables$var[[i]]
  
  # Number and proportion of contacts in each strata
  
  t0 <- as.data.frame(cbind(table(x), # Total 
                            round(prop.table(table(x))*100, digits=0) # Proportion
  )) # number of participants
  
  colnames(t0)[1:2] <- c("Total","Col") 
  Tot <- rep("",5)
  
  # Rural median contacts for day 1
  t1_median <- df_contact_d2 %>%
    dplyr::filter(study_site == "Rural") %>%
    # since this is total contacts per person, keep only 1 distinct record
    distinct(rec_id, .keep_all = TRUE) %>% 
    dplyr::group_by(.dots = participant_variables$var[[i]]) %>% 
    drop_na(participant_variables$var[[i]]) %>%
    do(data.frame(t(quantile(.$num_contacts, na.rm=TRUE, probs=c(0.25,0.5,0.75))))) # Median and IQR      
  t1_median$med_contact <- as.character(paste(
    t1_median$X50., " (",t1_median$X25., "-", t1_median$X75.,")", sep=""))
  
  # Calculate mean and 95% confidence intervals for day 1
  t1_mean <- df_contact_d2 %>%
    dplyr::filter(study_site == "Rural") %>%
    distinct(rec_id, .keep_all = TRUE) %>%
    dplyr::group_by(.dots = participant_variables$var[[i]]) %>%
    drop_na(participant_variables$var[[i]]) %>%
    summarise(
      mean_contact = round(mean(num_contacts, na.rm = TRUE), 1),
      lower_ci = round(t.test(num_contacts, na.rm = TRUE)$conf.int[1], 1),
      upper_ci = round(t.test(num_contacts, na.rm = TRUE)$conf.int[2], 1),
    )
  t1_mean$mean_contact <- as.character(paste(
    t1_mean$mean_contact," (",t1_mean$lower_ci, "-", t1_mean$upper_ci,")", sep=""))
 
  # Median contacts for day 2
  t2_median <- df_contact_d2 %>%
    dplyr::filter(study_site == "Urban") %>%
    # since this is total contacts per person, keep only 1 distinct record
    distinct(rec_id, .keep_all = TRUE) %>% 
    dplyr::group_by(.dots = participant_variables$var[[i]]) %>%
    drop_na(participant_variables$var[[i]]) %>%
    do(data.frame(t(quantile(.$num_contacts, na.rm=TRUE, probs=c(0.25,0.5,0.75))))) # Median and IQR      
  t2_median$med_contact <- as.character(paste(
    t2_median$X50.," (",t2_median$X25.,"-",t2_median$X75.,")",sep="")) # Formatting for export
  
  # Calculate mean and 95% confidence intervals for day 1
  t2_mean <- df_contact_d2 %>%
    dplyr::filter(study_site == "Urban") %>%
    distinct(rec_id, .keep_all = TRUE) %>%
    dplyr::group_by(.dots = participant_variables$var[[i]]) %>%
    drop_na(participant_variables$var[[i]]) %>%
    summarise(
      mean_contact = round(mean(num_contacts, na.rm = TRUE), 1),
      lower_ci = round(t.test(num_contacts, na.rm = TRUE)$conf.int[1], 1),
      upper_ci = round(t.test(num_contacts, na.rm = TRUE)$conf.int[2], 1),
    )
  t2_mean$mean_contact <- as.character(paste(
    t2_mean$mean_contact, " (",t2_mean$lower_ci, "-", t2_mean$upper_ci,")", sep=""))
  
  # Bind columns together and select relevant columns
  t0<-qpcR:::cbind.na(t0, t1_median$med_contact, t1_mean$mean_contact,
                      t2_median$med_contact, t2_mean$mean_contact)
  colnames(t0)[3:6] <- c("Rural Median", "Rural Mean", "Urban Median", "Urban Mean")
  
  # rename total column
  t0$Total <- paste(t0$Total," (","", t0$Col,")",sep="")
  t0 <- t0[,c(1,3,4,5,6)]
  t0[,1] <- as.character(t0[,1])
  t0[,2] <- as.character(t0[,2])
  
  t0<-rbind(Tot, t0)
  
  rownames(t0)[1] <- participant_variables$name[[i]]
  results_list[[i]] <- t0
}

table4 <- res_restruct(results_list)
colnames(table5) <- c("Total (%)", "Day 1", "Day 2")

table4 <- kable(table4, digits = 0, align = "r") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
# table4

rm(t0, t1, t2)

# @tbl-urban-contact-table shows a summary of the median (IQR) contacts in the urban area on day 1 and day 2 separately.
# We observe no difference in the median (IQR) number of contacts reported on day 1 compared to day 2.



#| label: fig-participant-summary-literacy
# read_write by age
literacy_page <- participants %>%
  group_by(read_write, participant_age, study_site) %>% 
  dplyr::summarise(total = n(), .groups = "drop") %>%
  na.omit()

literacy_page_plt <- fun_literacy_plot(literacy_page) +
  axis_text_theme2

# ggsave(literacy_page_plt, filename = "../../output/figs/1a_participant_literate.pdf",
#        height=6, width=8, dpi=300,
#        bg="#FFFFFF")

# literacy_page_plt



#| label: fig-participant-summary-enrolledsch
# enrolled_school by age, "Currently enrolled in school"
enrolledsch_page <- participants %>%
  group_by(enrolled_school, participant_age, study_site) %>% 
  dplyr::summarise(total = n(), .groups = "drop") %>%
  na.omit()

enrolledsch_page_plt <- fun_schenrolled_plot(enrolledsch_page)

# ggsave(enrolledsch_page_plt, filename = "../../output/figs/1a_participant_enrolledschool.pdf",
#        height=6, width=8, dpi=300,
#        bg="#FFFFFF")

# enrolledsch_page_plt



#| label: fig-participant-summary-educationlevel
# highest_educ by age <- "Highest education level attained"
highesteduc_page <- participants %>%
  group_by(highest_educ, participant_age, study_site) %>% 
  dplyr::summarise(total = n(), .groups = "drop") %>%
  na.omit()

highesteduc_page_plt <- fun_highesteduc_plot(highesteduc_page)

# ggsave(highesteduc_page_plt, filename = "../../output/figs/1a_participant_educlevel.pdf",
#        height=6, width=8, dpi=300,
#        bg="#FFFFFF")

# highesteduc_page_plt



#| label: fig-participant-summary-occupation
# occupation by age <- "Occupation"
occupation_page <- participants %>%
  group_by(occupation, participant_age, study_site) %>%
  dplyr::summarise(total = n(), .groups = "drop") %>%
  na.omit()

occupation_page_plt <- fun_occupation_plot(occupation_page)

# ggsave(occupation_page_plt, filename = "../../output/figs/1a_participant_occupation.pdf",
#        height=6, width=8, dpi=300,
#        bg="#FFFFFF")
#        



### TRIAL TO USE SOCIALMIXR PACKAGE
# library(socialmixr)
# 
# 
# rural_data <- df_contact_d1 %>%
#   filter(study_site == "Rural") %>%
#   select(rec_id, study_site, age, participant_age, participant_sex, contact_age, contact_sex, study_day) %>%
#   mutate(
#     country = "Mozambique",
#     contact_age = as.character(contact_age, participant_age),
#     cnt_age_est_min = case_when(
#       contact_age == "<6mo" ~ 0,
#       contact_age == "6-11mo" ~ 6,
#       contact_age == "1-4y" ~ 10,
#       contact_age == "5-9y" ~ 15,
#       contact_age == "10-14y" ~ 20,
#       contact_age == "15-19y" ~ 25,
#       contact_age == "20-29y" ~ 30,
#       contact_age == "30-39y" ~ 40,
#       contact_age == "40-59y" ~ 50,
#       contact_age == "60+y" ~ 60,
#       TRUE ~ NA_real_),
#     
#     cnt_age_est_max = case_when(
#       contact_age == "<6mo" ~ 5,
#       contact_age == "6-11mo" ~ 9,
#       contact_age == "1-4y" ~ 14,
#       contact_age == "5-9y" ~ 19,
#       contact_age == "10-14y" ~ 24,
#       contact_age == "15-19y" ~ 29,
#       contact_age == "20-29y" ~ 39,
#       contact_age == "30-39y" ~ 49,
#       contact_age == "40-59y" ~ 59,
#       contact_age == "60+y" ~ 100,
#       TRUE ~ NA_real_),
#   )
# 
# # age_limits =c(0,5,4,9,14,19,29,39,59)
# 
# rural_data <- survey(
#   participants = rural_data %>%
#     select(rec_id, age, participant_age, participant_sex, country) %>%
#     rename("part_age" = "age",
#            "part_gender" = "participant_sex",
#            "part_id" = "rec_id") %>%
#     # mutate(part_age = as.character(part_age)) %>%
#     unique(),
#   
#   contacts = rural_data %>%
#     dplyr::filter(study_day==1) %>%
#     select(rec_id, cnt_age_est_min, cnt_age_est_max, contact_sex) %>%
#     rename("cnt_gender" = "contact_sex",
#            "part_id" = "rec_id")
# )
# 
# socialmixr::contact_matrix(rural_data, 
#                            countries = "Mozambique",
#                            # age.limits = age_limits,
#                            missing.participant.age = NULL,
#                            symmetric = TRUE)
# 

cat("End of main analysis script")