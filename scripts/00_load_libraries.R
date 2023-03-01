## Install and load packages

rm(list=ls())

# install.packages("pacman") # install this package first if not installed

## Installing required packges
pacman::p_load(tidyr, dplyr, ggplot2, plotly,
               ggthemes, lubridate, knitr, kableExtra, 
               table1, kableExtra, gtsummary, readr)
# RCurl, REDCapR, redcapAPI, 
