

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


make_matrix <- function(df1, title, txt_size=12, mid=8, max = 15, legendpos="top") {
  df1 %>%
    select(-c(participant_age)) %>%
    group_by(participant_age2, contact_age) %>%
    rename(participant_age = participant_age2) %>%
    dplyr::summarize(tot_contacts = n()) %>% 
    full_join(standard_str, by= c("participant_age","contact_age"), keep=F) %>%
    # replace(is.na(.), 0) %>%
    left_join(aim1_n, by="participant_age") %>%
    mutate(avg_cont = (tot_contacts/n),
           avg_cont = replace_na(avg_cont, 0),
           avg_cont = na_if(avg_cont, 0)) %>%
    contactmatrix_viz(title=title, txt_size=txt_size, mid=mid, max=max, legendpos=legendpos)
}