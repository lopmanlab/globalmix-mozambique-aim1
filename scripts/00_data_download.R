#!/usr/bin/env Rscript

library(REDCapR)
token <- "B90C5E7F7911BC2189D8367CAA4D771B" # insert personal redcap token
url <- "https://redcap.emory.edu/api/"

data <- redcap_read(batch_size = 500L, interbatch_delay = 0.5, continue_on_error = TRUE, redcap_uri = url, token = token, records = NULL, 
                   records_collapsed = "", fields_collapsed = "")$data

# save file
<<<<<<< HEAD
rio::export(data, "raw/all_data_09242021.csv") ##### Save the file in your preferred directory
=======
rio::export(data, "raw/all_data_03022022.csv") ##### Save the file in your preferred directory


>>>>>>> cc7c3454b49acdff9c6b5e5f76f571e4a8f220c9
