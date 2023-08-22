
###############################################################################  
# This file contains common functions that are used in the analysis script.
# Author: Moses C Kiti, PhD
# 
###############################################################################  


# custom plot text theme option 1
axis_text_theme1 <- theme_classic() + 
  theme(
    plot.title = element_text(size = 12),
    axis.title.x = element_text(size=12, face="bold"),
    axis.title.y = element_text(size=12, face="bold"),
    axis.text.x = element_text(size = 10, angle=90, hjust = 1),
    axis.text.y = element_text(size= 10),
    legend.position = c(0.1, 0.7),
    legend.key.size = unit(1, "lines"),
    legend.direction = "horizontal",
    # legend.position = "top",
    # legend.justification = "left",
    legend.title = element_blank(),
    legend.box.background = element_rect(fill='transparent'),
    legend.background = element_rect(colour ="white"))

# custom plot text theme option 1
axis_text_theme2 <- theme_classic() + 
  theme(
    plot.title = element_text(size = 10),
    axis.title.x = element_text(size=10, face="bold"),
    axis.title.y = element_text(size=10, face="bold"),
    axis.text.x = element_text(size = 8, angle=0, hjust = 1),
    axis.text.y = element_text(size= 8),
    legend.position = c(0.2, 0.9),
    legend.key.size = unit(1, "lines"),
    legend.direction = "horizontal",
    # legend.position = "top",
    # legend.justification = "left",
    legend.title = element_blank(),
    legend.box.background = element_rect(fill='transparent'),
    legend.background = element_rect(colour ="white"))

## Generate boxplots
fxn_fig_boxplot <- function(data, fill_var) {
  # Create the ggplot boxplot
  plot <- ggplot(data, aes(x = participant_age, y = num_contacts, fill = !!rlang::sym(fill_var))) +
    geom_boxplot(width = 0.4, position = position_dodge(width = 0.5)) +
    facet_wrap(~study_site, nrow = 2) +
    scale_fill_manual(values = cols_site) +
    labs(x = "Participant age", y = "Number of contacts") +
    ylim(0, 40) +
    theme_classic() +
    axis_text_theme1
  
  return(plot)
}

## Functions
## Function used to save legend of ggplot2 (allows manipulating legend)
get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

## Function used to visualize age-specific contact mixing matrix with controls 
## over title, text size, mid and max points for legend and legend position

contactmatrix_viz <- function(matrix1, title, txt_size, mid, max, legendpos) {
  ggplot(data = matrix1, aes(x=factor(participant_age), y=factor(contact_age), fill=avg_cont)) +  
    ## x is age of participant, y is age of contact
    geom_raster(hjust = 0.5, vjust = 0.5, show.legend=T) +
    scale_colour_discrete(na.translate = F) +
    scale_fill_gradient2(low = "#ffffcc", mid = "#fd8d3c", high = "#bd0026", 
                         midpoint = mid, limit = c(0, max)) +
    xlab("Participant age") + 
    ylab("Contact age") + 
    labs(fill = "Avg \ncontact") +
    theme_classic() +
    theme(legend.title = element_text(size = 10),
          legend.text = element_text(size = 10),
          legend.justification = "right",
          legend.position = legendpos) +
    theme(plot.title = element_text(size = 10), 
          axis.title.x = element_text(size=10, face="bold", angle=0),
          axis.title.y = element_text(size=10, face="bold"),
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size= 10)) +
    ggtitle(title)
}


make_matrix <- function(df1, site, title, txt_size=12, mid=8, max = 15, legendpos="top") {
  df1 %>%
    filter(study_site == study_site) %>%
    group_by(participant_age, contact_age) %>%
    dplyr::summarize(tot_contacts = n()) %>% 
    full_join(standard_str, by= c("participant_age", "contact_age"), keep=F) %>%
    replace(is.na(.), 0) %>%
    left_join(n_participants, by="participant_age") %>%
    mutate(avg_cont = (tot_contacts/n),
           avg_cont = replace_na(avg_cont, 0),
           avg_cont = na_if(avg_cont, 0)) %>%
    contactmatrix_viz(title=title, txt_size=txt_size, mid=mid, max=max, legendpos=legendpos)
}
