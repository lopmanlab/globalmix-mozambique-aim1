# Figure 1 images
# 

## histograms
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
                 hjust=0, size=4) +
        labs(title = "Rural site", # Rural
             x="",
             y="Frequency") + # Frequency
        axis_text_theme2

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
                 hjust=0, size=4) +
        labs(title = "Urban site", # Urban
             x="",
             y="Frequency") +
        axis_text_theme2

## box plot
fig_contacts_age_box <- ggplot(contacts_age, aes(x = participant_age, 
                                                 y = num_contacts, 
                                                 fill = study_site)) +
        geom_boxplot(width = 0.4, position = position_dodge(width = 0.5), outlier.shape = NA) +
        scale_fill_manual(values = cols_site) +
        labs(x = "Participant age", y = "No. of contacts") +
        ylim(0, 30) +
        theme_classic() +
        labs(title = "Age-specific contact distribution by site")
        axis_text_theme2 

## matrices
rural_symmetric_matrix <- fun_symmetric_matrix(df_contact_d1, "Rural", n_participants_rural)
rural_symmetric_matrix_plot <- fun_symmetric_plot(rural_symmetric_matrix, "Rural site","Participant age","Contact age")
matrix_legend <- get_legend(rural_symmetric_matrix_plot)

urban_symmetric_matrix <- fun_symmetric_matrix(df_contact_d1, "Urban", n_participants_urban)
urban_symmetric_matrix_plot <- fun_symmetric_plot(urban_symmetric_matrix, "Urban site", "Participant age","Contact age")


ggsave(contact_hist_rural_d1, filename = "../../output/figs/Figure 1A.jpg",
       height=6, width=8, dpi=300,
       bg="#FFFFFF")

ggsave(contact_hist_urban_d1, filename = "../../output/figs/Figure 1B.jpg",
       height=6, width=8, dpi=300,
       bg="#FFFFFF")

ggsave(fig_contacts_age_box, filename = "../../output/figs/Figure 1C.jpg",
       height=6, width=8, dpi=300,
       bg="#FFFFFF")

ggsave(rural_symmetric_matrix_plot, filename = "../../output/figs/Figure 1D.jpg",
       height=6, width=8, dpi=300,
       bg="#FFFFFF")

ggsave(urban_symmetric_matrix_plot, filename = "../../output/figs/Figure 1E.jpg",
       height=6, width=8, dpi=300,
       bg="#FFFFFF")


# Figure 2 images
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
             title = "Rural site") +
        geom_text(aes(label=round(contacts, digits=1)), 
                  colour = "black", check_overlap = TRUE, size=4) +
        axis_text_theme2 +
        theme(legend.position = "right",
              legend.direction = "vertical",
              axis.text.x = element_text(size = 10, angle=90, hjust = 1))

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
             title = "Urban site") +
        geom_text(aes(label=round(contacts, digits=1)), colour = "black", 
                  check_overlap = TRUE, size=4) +
        axis_text_theme2 +
        theme(legend.position = "right",
              legend.direction = "vertical",
              axis.text.x = element_text(size = 10, angle=90, hjust = 1))

premmatrix <- ggplot(moz_prem10, aes(x = part_age2, y = cont_age2, fill = contacts)) + 
        geom_tile() +
        scale_fill_gradient2(low = "#91bfdb", mid="#fee090", high="#d73027",
                             midpoint = 10, limit = c(0,20))+
        labs(x = "Participant age",
             y = "Contact age",
             title = "Prem et al.") +
        geom_text(aes(label=round(contacts, digits=1)), 
                  colour = "black", check_overlap = TRUE, size=4) +
        axis_text_theme2 +
        theme(axis.text.x = element_text(size = 10, angle=90, hjust = 1),
              legend.position = "right",
              legend.direction = "vertical")

ggsave(ruralmatrix, filename = "../../output/figs/Figure 2A.jpg",
       height=6, width=8, dpi=300,
       bg="#FFFFFF")

ggsave(urbanmatrix, filename = "../../output/figs/Figure 2B.jpg",
       height=6, width=8, dpi=300,
       bg="#FFFFFF") 

ggsave(premmatrix, filename = "../../output/figs/Figure 2C.jpg",
       height=6, width=8, dpi=300,
       bg="#FFFFFF")

ggsave(oe_rural_v3, filename = "../../output/figs/Figure 2D.jpg",
       height=6, width=8, dpi=300,
       bg="#FFFFFF")

ggsave(oe_urban_v3, filename = "../../output/figs/Figure 2E.jpg",
       height=6, width=8, dpi=300,
       bg="#FFFFFF")

# Supplementary figure 1
rural_matrix_touch <- fun_matrix2_plot(m1_rural_touch, "Rural physical contacts",
                                       xlab = "Participant age", ylab = "Contact age") + 
        theme(legend.position = "right",
              legend.direction = "vertical",
              axis.text.x = element_text(angle=90))

rural_matrix_conv <- fun_matrix2_plot(m1_rural_conv, "Rural conversation contacts", 
                                      xlab = "Participant age", ylab = "Contact age")  +
        theme(legend.position = "right",
              legend.direction = "vertical",
              axis.text.x = element_text(angle=90))

urban_matrix_touch <- fun_matrix2_plot(m1_urban_touch, "B. Urban physical", 
                                       xlab = "Participant age", ylab = "Contact age") +
        theme(legend.position = "right",
              legend.direction = "vertical",
              axis.text.x = element_text(angle=90))

urban_matrix_conv <- fun_matrix2_plot(m1_urban_conv, "Urban conversation contacts", 
                                      xlab = "Participant age", ylab = "Contact age") +
        theme(legend.position = "right",
              legend.direction = "vertical",
              axis.text.x = element_text(angle=90))

ggsave(rural_matrix_touch, filename = "../../output/figs/Figure SI 1A.jpg",
       height=6, width=8, dpi=300,
       bg="#FFFFFF")

ggsave(urban_matrix_touch, filename = "../../output/figs/Figure SI 1B.jpg",
       height=6, width=8, dpi=300,
       bg="#FFFFFF")

ggsave(rural_matrix_conv, filename = "../../output/figs/Figure SI 1C.jpg",
       height=6, width=8, dpi=300,
       bg="#FFFFFF")

ggsave(urban_matrix_conv, filename = "../../output/figs/Figure SI 1D.jpg",
       height=6, width=8, dpi=300,
       bg="#FFFFFF")

