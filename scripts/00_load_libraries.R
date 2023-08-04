## Install and load packages

rm(list=ls())

# install.packages("pacman") # install this package first if not installed

## Installing required packges
pacman::p_load(dplyr, ggplot2, ggthemes, gtsummary, 
               knitr, kableExtra, lubridate,
               patchwork, readr, plotly, table1, tidyr)
# RCurl, REDCapR, redcapAPI, 
