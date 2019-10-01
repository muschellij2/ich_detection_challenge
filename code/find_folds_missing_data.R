rm(list = ls())
library(dcm2niir)
library(divest)
library(neurobase)
library(ichseg)
library(dplyr)
library(fs)
library(dcmtk)
setwd(here::here())

add_instance_number = TRUE

sub_bracket = function(x) {
  x = sub("^\\[", "", x)
  x = sub("\\]$", "", x)
  x = trimws(x)
}

tmp = sapply(c("ss", "mask", "nifti"), dir.create, 
             showWarnings = FALSE)

n_folds = 200

all_df = readr::read_rds("wide_headers_with_folds.rds")
all_df = all_df %>% 
  select(index, outfile, fold) %>% 
  distinct()

ifold = 1
for (ifold in 1:n_folds) {
  # print(ifold)
  df = all_df %>% 
    filter(fold == ifold) 
  fe = sum(!file.exists(df$outfile))
  if (fe > 0) {
    print(paste0("ifold: ", ifold, ", missing: ", fe))
  }
}
