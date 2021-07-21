#!/usr/bin/env Rscript

# source('config.R')
library(RCurl)

result <- postForm(
  api_url,
  token=api_token,
  content='record',
  format='json',
  type='flat'
)
print(result)