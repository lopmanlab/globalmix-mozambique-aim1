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
    
    theme(legend.position = "bottom",
          # legend.title = element_text(size = 8),
          legend.text = element_text(size = 10),
          legend.justification = "center") +
    # change legend order
    
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
    
    theme(legend.position = "bottom",
          legend.title = element_text(size = 10),
          legend.text = element_text(size = 6),
          legend.justification = "center") +
    # change legend order
    
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
    
    theme(legend.position = "bottom",
          # legend.title = element_text(size = 8),
          legend.text = element_text(size = 10),
          legend.justification = "center") +
    # change legend order
    
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
    
    theme(legend.position = "bottom",
          # legend.title = element_text(size = 8),
          legend.text = element_text(size = 10),
          legend.justification = "center") +
    # change legend order
    
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
    
    theme(legend.position = "bottom",
          legend.title = element_text(size = 8),
          legend.text = element_text(size = 10),
          legend.justification = "center") +
    # change legend order
    
    theme(plot.title = element_text(size = 14), 
          axis.title.x = element_text(size=12, face="bold"),
          axis.title.y = element_text(size=12, face="bold"),
          axis.text.x = element_text(size = 8, angle=0, hjust = 1),
          axis.text.y = element_text(size= 8))
}