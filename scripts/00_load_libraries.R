## Install and load packages

rm(list=ls())

# install.packages("pacman") # install this package first if not installed

## Installing required packges
pacman::p_load(tidyr, RColorBrewer, dplyr, leaflet,
               ggthemes, lubridate, plotly, knitr, kableExtra, ggplot2, forcats,
               flexdashboard, igraph, table1, kableExtra, gtsummary)
# RCurl, REDCapR, redcapAPI, 
