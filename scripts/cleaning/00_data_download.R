#!/usr/bin/env Rscript

library(REDCapR)
# token <- "B90C5E7F7911BC2189D8367CAA4D771B" # insert personal redcap token

token <- "F7053C0675C0AA86F897230E4FD84C38" # need to insert updated token.
#"5B0CEE7AE22F71A04B7C863E0F458249"
url <- "https://redcap.emory.edu/api/"

data <- redcap_read(batch_size = 500L, interbatch_delay = 0.5, 
                    continue_on_error = TRUE, redcap_uri = url, 
                    token = token, records = NULL, 
                   records_collapsed = "", 
                   fields_collapsed = "")$data

# save file
rio::export(data, "data/raw/all_data_09142022.RDS") 
##### Save the file in your preferred directory

# previous data downloaded on 07/05/2022
# # new data downloaded on 08092022
