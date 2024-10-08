###############################################################################  
# This file contains common functions that are used in the analysis script.
# Author: Moses C Kiti, PhD
# 
###############################################################################  

# rm(list=ls())

## 1. Install and load packages

# install.packages("pacman") # install this package first if not installed

## Installing required packages
# install_packages <- c("pacman", "processx", "EpiModel", "qpcR")
# if (!require(install_packages)) install.packages(install_packages)

set.seed(30322)

pacman::p_load(cowplot, dplyr, EpiModel, ggplot2, ggthemes, ggpubr, grid, 
               gridExtra, gtsummary, knitr, kableExtra, lubridate, patchwork, 
               plotly, readr, socialmixr, table1, tidyr, openxlsx)

cols_model <- c("#9467bd","#aec7e8", "#000000", "#000000")


# install orca to save plotly plots: https://github.com/plotly/orca#installation

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

# custom plot text theme option 2
axis_text_theme2 <- theme_classic() + 
  theme(
    plot.title = element_text(size = 14),
    axis.title.x = element_text(size=14, face="bold"),
    axis.title.y = element_text(size=14, face="bold"),
    axis.text.x = element_text(size = 10, angle=0, hjust = 1),
    axis.text.y = element_text(size= 10),
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

###++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## 2. Functions
## Function used to save legend of ggplot2 (allows manipulating legend)
get_legend <- function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

## Function used to visualize age-specific contact mixing matrix with controls 
## over title, text size, mid and max points for legend and legend position

# function to generate overall matrices
fun_matrix1_plot <- function(m1data, title, xlab, ylab){
  m1data %>%
    ggplot(aes(x = participant_age, y = contact_age, fill=average_contact)) +
    geom_raster() +
    geom_text(aes(participant_age, contact_age, label = average_contact), 
              color = "black", size = 3) +
    theme_classic() +
    scale_fill_gradient2(low="#91bfdb", mid="#fee090", high="#d73027", 
                         midpoint = 5, limits=c(0,10), breaks=(c(0,2,4,6,8,10))) +
    labs(x = xlab, 
         y = ylab,
         title = title,
         fill = "Average\ncontacts") +
    theme(legend.title = element_text(size = 10),
          legend.text = element_text(size = 8),
          legend.justification = "right") +
    theme(plot.title = element_text(size = 14), 
          axis.title.x = element_text(size=14, face="bold"),
          axis.title.y = element_text(size=14, face="bold"),
          axis.text.x = element_text(size = 10, angle=90),
          axis.text.y = element_text(size= 10))
}

# function to matrices by touch
fun_matrix2_plot <- function(m1data, title, xlab, ylab){
  m1data %>%
    ggplot(aes(x = participant_age, y = contact_age, fill=average_contact)) +
    geom_raster() +
    geom_text(aes(participant_age, contact_age, label = average_contact), 
              color = "black", size = 3) +
    theme_classic() +
    scale_fill_gradient2(low="#91bfdb", mid="#fee090", high="#d73027", 
                         midpoint = 2.5, limits=c(0,5), breaks=(c(0,1,3,5))) +
    labs(x = xlab, 
         y = ylab,
         title = title,
         fill = "Average\ncontacts") +
    theme(legend.title = element_text(size = 10),
          legend.text = element_text(size = 8),
          legend.justification = "right") +
    theme(plot.title = element_text(size = 14), 
          axis.title.x = element_text(size=14, face="bold"),
          axis.title.y = element_text(size=14, face="bold"),
          axis.text.x = element_text(size = 10, angle=90),
          axis.text.y = element_text(size= 10))
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


### Function to generate symmetric matrices

#### function to adjust for reciprocity
adjust_for_reciprocity <- function(matrix, num_participants) {
  n <- nrow(matrix)
  t <- as.numeric(num_participants$n_participants)
  
  # # Ensure the matrix is square
  # if (n != ncol(matrix)) {
  #   stop("Input matrix is not square.")
  # }
  
  # Adjust for reciprocity
  for (i in 1:n) {
    for (j in 1:n) {
      # do not adjust the diagonal
      # if (i != j) {
      matrix[i, j] <- ((matrix[i, j] + matrix[j, i]) / 
                         (num_participants$n_participants[i] + num_participants$n_participants[j]))
        # here m.t is number of participants in the age group
      # }
    }
  }
  
  return(matrix)
}

#### generating symmetric matrix
fun_symmetric_matrix <- function(data, study_site, n){
  # load data
  matrix_data <- data %>%
    dplyr::filter(study_site == {{study_site}}) %>%
    dplyr::group_by(participant_age, contact_age) %>%
    dplyr::summarize(total_contacts = n()) %>%
    full_join(standard_str, by = c("participant_age", "contact_age"), keep = F) %>%
    dplyr::mutate(total_contacts = replace_na(total_contacts, 0)) %>% 
    drop_na() %>%
    distinct(participant_age, contact_age, .keep_all = T)  
  
  # generate table of total number of contacts by age
  contact_matrix <- xtabs(total_contacts ~ participant_age + contact_age, data = matrix_data)
  # print(contact_matrix)
  
  # adjust for reciprocity using function defined
  symmetric_matrix <- adjust_for_reciprocity(contact_matrix, n)
  
  # compute average
  symmetric_matrix <- reshape2::melt(symmetric_matrix) %>%
    rename(average_contact = value) %>%
    mutate(average_contact = round(average_contact, 1))
  
  # print(xtabs(average_contact ~ participant_age + contact_age, data = symmetric_matrix))
  # left_join(n, by="participant_age")  %>%
  # dplyr::mutate(average_contact = round(value / n$n_participants, 
  #                                       digits = 1))
  
  return(symmetric_matrix)
}  


#### plot the matrix
fun_symmetric_plot <- function(data, title, xlab, ylab){
  data %>%
    ggplot(aes(x = participant_age, y = contact_age, fill=average_contact)) +
    geom_raster() +
    geom_text(aes(participant_age, contact_age, label = average_contact), 
              color = "black", size = 2) +
    theme_classic() +
    scale_fill_gradient2(low="#91bfdb", mid="#fee090", high="#d73027", 
                         midpoint = 3, limits=c(0,6), breaks=(c(0,2,4,6))) +
    labs(x = xlab, 
         y = ylab,
         title = title,
         fill = "Average\ncontacts") +
    theme(legend.title = element_text(size = 10),
          legend.text = element_text(size = 8),
          legend.justification = "right") +
    theme(plot.title = element_text(size = 14), 
          axis.title.x = element_text(size=14, face="bold"),
          axis.title.y = element_text(size=14, face="bold"),
          axis.text.x = element_text(size = 10, angle=90),
          axis.text.y = element_text(size= 10))
}


## 3. Functions for additional plot customization

fun_sex_plot <- function(data){
  data %>%
    ggplot(aes(x = total, y = participant_age, fill = participant_sex, label = total)) +
    geom_bar(aes(x = total, y = participant_age, fill = participant_sex), 
             position = "fill", col="white", stat="identity") +
    geom_text(size=3, position=position_stack(vjust = 0.5)) +
    
    theme_classic() +
    facet_wrap(~study_site) +
    
    theme(strip.background = element_blank(),
          # strip.text.x = element_blank(), # remove facet titles
          panel.spacing = unit(2, "lines")) +  # increase space between facets
    
    coord_cartesian(xlim = c(0, 1), 
                    # ylim = c(0.5, 20.5), 
                    expand = F, # removes white spaces at edge of plot
                    clip = 'off') + # allows drawing outside of panel
    
    # move x-axis title to the top and format
    scale_x_continuous(position="top",
                       labels = function(x) format(x*100, digits=0, nsmall=0),
                       breaks = seq(0, 1, 0.2)) + 
    theme(axis.line.x = element_line(colour = "black"), 
          axis.ticks.x = element_line(colour = "black")) +
    
    labs(title = "",
         x = "% sex", 
         y = "Participant age",
         fill = element_blank()) +
    
    theme(legend.key.size = unit(1, "lines"),
          legend.direction = "horizontal",
          legend.position = c(0.1, 0.9),
          # legend.position = "bottom",
          legend.title = element_text(size = 8),
          legend.text = element_text(size = 10),
          # legend.justification = "center"
    ) +
    
    theme(plot.title = element_text(size = 14), 
          axis.title.x = element_text(size=12, face="bold"),
          axis.title.y = element_text(size=12, face="bold"),
          axis.text.x = element_text(size = 8, angle=0, hjust = 1),
          axis.text.y = element_text(size= 8))
}

# +++++ literacy
fun_literacy_plot <- function(data){
  data %>%
    ggplot(aes(x = total, y = participant_age, fill = read_write)) +
    geom_bar(aes(x = total, y = participant_age, fill = read_write), 
             position = "fill", col="white", stat="identity") +
    theme_classic() +
    facet_wrap(~study_site) +
    
    theme(strip.background = element_blank(),
          # strip.text.x = element_blank(), # remove facet titles
          panel.spacing = unit(2, "lines")) +  # increase space between facets
    
    coord_cartesian(xlim = c(0, 1), 
                    # ylim = c(0.5, 20.5), 
                    expand = F, # removes white spaces at edge of plot
                    clip = 'off') + # allows drawing outside of panel
    
    # move x-axis title to the top and format
    scale_x_continuous(position="top",
                       labels = function(x) format(x*100, digits=0, nsmall=0),
                       breaks = seq(0, 1, 0.2)) + 
    theme(axis.line.x = element_line(colour = "black"), 
          axis.ticks.x = element_line(colour = "black")) +
    
    labs(title = "",
         x = "% literate", 
         y = "Participant age",
         fill = element_blank()) +
    
    theme(legend.key.size = unit(1, "lines"),
          legend.direction = "horizontal",
          legend.position = c(0.1, 0.9),
          # legend.position = "bottom",
          legend.title = element_text(size = 8),
          legend.text = element_text(size = 10),
          # legend.justification = "center"
    ) +
    
    theme(plot.title = element_text(size = 14), 
          axis.title.x = element_text(size=12, face="bold"),
          axis.title.y = element_text(size=12, face="bold"),
          axis.text.x = element_text(size = 8, angle=0, hjust = 1),
          axis.text.y = element_text(size= 8))
}


# +++++ school enrollment
fun_schenrolled_plot <- function(data){
  data %>%
    ggplot(aes(x = total, y = participant_age, fill = enrolled_school)) +
    geom_bar(aes(x = total, y = participant_age, fill = enrolled_school), 
             position = "fill", col="white", stat="identity") +
    theme_classic() +
    facet_wrap(~study_site) +
    
    theme(strip.background = element_blank(),
          # strip.text.x = element_blank(), # remove facet titles
          panel.spacing = unit(2, "lines")) +  # increase space between facets
    
    coord_cartesian(xlim = c(0, 1), 
                    # ylim = c(0.5, 20.5), 
                    expand = F, # removes white spaces at edge of plot
                    clip = 'off') + # allows drawing outside of panel
    
    # move x-axis title to the top and format
    scale_x_continuous(position="top",
                       labels = function(x) format(x*100, digits=0, nsmall=0),
                       breaks = seq(0, 1, 0.2)) + 
    theme(axis.line.x = element_line(colour = "black"), 
          axis.ticks.x = element_line(colour = "black")) +
    
    labs(title = "",
         x = "% in school", 
         y = "Participant age",
         fill = element_blank()) +
    
    theme(legend.key.size = unit(1, "lines"),
          legend.direction = "horizontal",
          legend.position = c(0.1, 0.9),
          # legend.position = "bottom",
          legend.title = element_text(size = 8),
          legend.text = element_text(size = 10),
          # legend.justification = "center"
    ) +
    
    theme(plot.title = element_text(size = 14), 
          axis.title.x = element_text(size=12, face="bold"),
          axis.title.y = element_text(size=12, face="bold"),
          axis.text.x = element_text(size = 8, angle=0, hjust = 1),
          axis.text.y = element_text(size= 8))
}

# +++++
fun_highesteduc_plot <- function(data){
  data %>%
    ggplot(aes(x = total, y = participant_age, fill = highest_educ)) +
    geom_bar(aes(x = total, y = participant_age, fill = highest_educ), 
             position = "fill", col="white", stat="identity") +
    theme_classic() +
    facet_wrap(~study_site) +
    
    theme(strip.background = element_blank(),
          # strip.text.x = element_blank(), # remove facet titles
          panel.spacing = unit(2, "lines")) +  # increase space between facets
    
    coord_cartesian(xlim = c(0, 1), 
                    # ylim = c(0.5, 20.5), 
                    expand = F, # removes white spaces at edge of plot
                    clip = 'off') + # allows drawing outside of panel
    
    # move x-axis title to the top and format
    scale_x_continuous(position="top",
                       labels = function(x) format(x*100, digits=0, nsmall=0),
                       breaks = seq(0, 1, 0.2)) + 
    theme(axis.line.x = element_line(colour = "black"), 
          axis.ticks.x = element_line(colour = "black")) +
    
    labs(title = "",
         x = "% highest education level attained", 
         y = "Participant age",
         fill = element_blank()) +
    
    theme(legend.key.size = unit(1, "lines"),
          legend.direction = "horizontal",
          legend.position = c(0.1, 0.9),
          # legend.position = "bottom",
          legend.title = element_text(size = 8),
          legend.text = element_text(size = 10),
          # legend.justification = "center"
    ) +
    
    theme(plot.title = element_text(size = 14), 
          axis.title.x = element_text(size=12, face="bold"),
          axis.title.y = element_text(size=12, face="bold"),
          axis.text.x = element_text(size = 8, angle=0, hjust = 1),
          axis.text.y = element_text(size= 8))
}


# +++++ occupation
fun_occupation_plot <- function(data){
  data %>%
    ggplot(aes(x = total, y = participant_age, fill = occupation)) +
    geom_bar(aes(x = total, y = participant_age, fill = occupation), 
             position = "fill", col="white", stat="identity") +
    theme_classic() +
    facet_wrap(~study_site) +
    
    theme(strip.background = element_blank(),
          # strip.text.x = element_blank(), # remove facet titles
          panel.spacing = unit(2, "lines")) +  # increase space between facets
    
    coord_cartesian(xlim = c(0, 1), 
                    # ylim = c(0.5, 20.5), 
                    expand = F, # removes white spaces at edge of plot
                    clip = 'off') + # allows drawing outside of panel
    
    # move x-axis title to the top and format
    scale_x_continuous(position="top",
                       labels = function(x) format(x*100, digits=0, nsmall=0),
                       breaks = seq(0, 1, 0.2)) + 
    theme(axis.line.x = element_line(colour = "black"), 
          axis.ticks.x = element_line(colour = "black")) +
    
    labs(title = "",
         x = "% occupation", 
         y = "Participant age",
         fill = element_blank()) +
    
    theme(legend.key.size = unit(1, "lines"),
          legend.direction = "horizontal",
          legend.position = c(0.1, 0.9),
          # legend.position = "bottom",
          legend.title = element_text(size = 8),
          legend.text = element_text(size = 10),
          # legend.justification = "center"
    ) +
    # change legend order
    
    theme(plot.title = element_text(size = 14), 
          axis.title.x = element_text(size=12, face="bold"),
          axis.title.y = element_text(size=12, face="bold"),
          axis.text.x = element_text(size = 8, angle=0, hjust = 1),
          axis.text.y = element_text(size= 8))
}


#+++++++++++
fun_contact_behavior <- function(df, action){
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
          axis.ticks.x = element_line(colour = "black"),
          legend.box = "vertical") # +
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
