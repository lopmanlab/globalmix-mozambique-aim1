# custom plot text theme option 2
axis_text_theme3 <- theme_classic() + 
  theme(
    plot.title = element_text(size = 22),
    axis.title.x = element_text(size=20, face="bold"),
    axis.title.y = element_text(size=20, face="bold"),
    axis.text.x = element_text(size = 18, angle=0, hjust = 1),
    axis.text.y = element_text(size= 18),
    # legend.position = c(0.2, 0.9),
    legend.key.size = unit(1, "lines"),
    # legend.direction = "horizontal",
    # legend.position = "top",
    # legend.justification = "left",
    legend.title = element_blank(),
    legend.box.background = element_rect(fill='transparent'),
    legend.background = element_rect(colour ="white"))


fun_matrix2_plot <- function(m1data, title, xlab, ylab){
  m1data %>%
    ggplot(aes(x = participant_age, y = contact_age, fill=average_contact)) +
    geom_raster() +
    geom_text(aes(participant_age, contact_age, label = average_contact), 
              color = "black", size = 5) +
    theme_classic() +
    scale_fill_gradient2(low="#91bfdb", mid="#fee090", high="#d73027", 
                         midpoint = 4, limits=c(0,8), breaks=(c(0,2,4,6,8))) +
    labs(x = xlab, 
         y = ylab,
         title = title,
         fill = "Average\ncontacts") +
    theme(legend.title = element_text(size = 10),
          legend.text = element_text(size = 20),
          legend.position = "right",
          plot.title = element_text(size = 22), 
          axis.title.x = element_text(size=22, face="bold"),
          axis.title.y = element_text(size=22, face="bold"),
          axis.text.x = element_text(size = 20, angle=90),
          axis.text.y = element_text(size= 20))
}


# Age and sex distribution of participants
fig_sex_age_p <- sex_page %>%
  ggplot(., aes(x=participant_age, y=total, fill=participant_sex)) +
  geom_bar(aes(x = participant_age, y = total, fill = participant_sex), 
           position = "stack", col="white", stat="identity") +
  scale_fill_manual(values = cols_sex) +  # Set custom fill colors
  facet_wrap(~study_site) +
  annotation_custom(grobTree(textGrob("Target=63", x=0.6, y=0.757, hjust=-0.2,
                                      gp=gpar(fontsize=20)))) +
  coord_flip() +
  
  labs(title = "",
       x = "Participant age", 
       y = "N",
       fill = element_blank()) +
  axis_text_theme3 +
  theme(
    axis.title.x = element_text(size= 24, face="bold"),
    axis.title.y = element_text(size= 24, face="bold"),
    axis.text.x = element_text(size = 20, angle=0, hjust = 1),
    axis.text.y = element_text(size= 20),
    legend.position = c(0.4, 0.4),
    legend.key.size = unit(2, "lines"),
    legend.text = element_text(size = 20),
    strip.background = element_blank(),
    legend.direction  = "vertical",
    # strip.text = element_blank(), # remove facet titles
    panel.spacing = unit(2, "lines")) +
  geom_hline(yintercept=63, linetype="dashed")

# fig_sex_age
ggsave(fig_sex_age_p, filename = "output/figs/presentation/fig_participant_age_sex_p.pdf",
       height=6, width=10, dpi=300,
       bg="#FFFFFF")



# frequency distribution of contacts
contact_hist_rural_d1_p <- contacts_site_d1 %>%
  dplyr::filter(study_site == "Urban") %>%
  ggplot(., aes(x=num_contacts)) +
  geom_density(color = cols_site[1], fill = cols_site[1]) +
  # scale_fill_manual(values = cols_site[1]) +
  scale_x_continuous(limits = c(0, 40)) +
  scale_y_continuous(limits = c(0, 0.15)) +
  geom_vline(xintercept=mean_contacts_site_d1$mean[1], color="black") +
  annotate("text", x=mean_contacts_site_d1$mean[1]+1, y=0.08,
           label=paste0("Mean= ", mean_contacts_site_d1$mean[1],
                        " (95% CI ", ci_contacts_rural_d1$ll,"-", #" (95% CI ", 
                        ci_contacts_rural_d1$ul,")"),
           hjust=0, size=8) +
  labs(title = "Rural", # Rural
       x="",
       y="Contact density") + # Frequency
  axis_text_theme3
contact_hist_rural_d1_p
# theme(axis.text.x = element_text(angle=0))
# hist_rural

## urban histogram
contact_hist_urban_d1_p <- contacts_site_d1 %>%
  dplyr::filter(study_site == "Urban") %>%
  ggplot(., aes(x=num_contacts)) +
  geom_density(color = cols_site[2], fill = cols_site[2]) +
  # scale_fill_manual(values = cols_site[2]) +
  scale_x_continuous(limits = c(0, 40)) +
  scale_y_continuous(limits = c(0, 0.15)) +
  geom_vline(xintercept=mean_contacts_site_d1$mean[2], color="black") +
  annotate("text", x=mean_contacts_site_d1$mean[2]+3, y=0.08,
           label=paste0("Mean= ", mean_contacts_site_d1$mean[2],
                        " (95% CI ", ci_contacts_urban_d1$ll,"-", # " (95% CI ", 
                        ci_contacts_urban_d1$ul,")"),
           hjust=0, size=8) +
  labs(title = "Urban", # Urban
       y="Contact density",
       x="") +
  axis_text_theme3
contact_hist_urban_d1_p
# theme(axis.text.x = element_text(angle=0))
# hist_urban

contact_hist_d1_p <- contact_hist_rural_d1_p / contact_hist_urban_d1_p

ggsave(contact_hist_d1_p, 
       filename = "output/figs/presentation/fig_contact_hist_overall_p.pdf",
       height=6, width=8, dpi=300,
       bg="#FFFFFF")



# distribution of contacts by age and sex
fig_contacts_age_box_p <- ggplot(contacts_age, aes(x = participant_age, 
                                                   y = num_contacts, 
                                                   fill = study_site)) +
  geom_boxplot(width = 0.4, position = position_dodge(width = 0.5), outlier.shape = NA) +
  scale_fill_manual(values = cols_site) +
  labs(x = "Participant age", y = "Number of contacts") +
  ylim(0, 30) +
  theme_classic() +
  axis_text_theme3 +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(legend.position = "none") 

ggsave(fig_contacts_age_box_p, filename = "output/figs/presentation/fig_contact_age_box_p.pdf",
       height=4, width=8, dpi=300,
       bg="#FFFFFF")



#| fig-cap: "Crude contact matrices in rural and urban Mozambique"
# Rural site
rural_matrix_p <- fun_matrix2_plot(m1_rural, "Rural (Q-index = 0.12)", 
                                   xlab = "", ylab = "Contact age")
matrix_legend_p <- get_legend(rural_matrix_p)
rural_matrix_p <- rural_matrix_p +
  axis_text_theme3 +  
theme(legend.position = "right",
      legend.direction = "vertical",
      axis.text.x =  element_blank())
rural_matrix

# Urban site
urban_matrix_p <- fun_matrix2_plot(m1_urban, "Urban (Q-index = 0.23)", 
                                   xlab = "Participant age", ylab = "Contact age") +
  axis_text_theme3 + 
  theme(legend.position = "none",
        axis.text.x =  element_text(angle=90))
urban_matrix_p

fig_crude_matrix_p <- rural_matrix_p / urban_matrix_p
fig_crude_matrix_p

ggsave(fig_crude_matrix_p, filename = "output/figs/presentation/fig_crude_matrix_p.pdf",
       height=12, width=8, dpi=300,
       bg="#FFFFFF")
