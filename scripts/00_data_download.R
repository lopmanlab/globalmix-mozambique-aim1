#!/usr/bin/env Rscript

library(REDCapR)
token <- "5B0CEE7AE22F71A04B7C863E0F458249"
url <- "https://redcap.emory.edu/api/"

data <- redcap_read(batch_size = 500L, interbatch_delay = 0.5, continue_on_error = TRUE, redcap_uri = url, token = token, records = NULL, 
                   records_collapsed = "", fields_collapsed = "")$data

# save file
rio::export(data, "raw/all_data_09242021.csv") ##### Save the file in your preferred directory
