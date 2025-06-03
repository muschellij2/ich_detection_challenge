rm(list = ls())
library(neurobase)
library(dplyr)
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
  select(index, outfile, fold, ss_file, ss_robust_file, maskfile) %>% 
  distinct()

ifold = 1
for (ifold in 1:n_folds) {
  # print(ifold)
  df = all_df %>% 
    filter(fold == ifold) 
  # fe = sum(!file.exists(df$outfile))
  # if (fe > 0) {
  #   print(paste0("ifold: ", ifold, ", missing: ", fe))
  # }
  # fe = sum(!file.exists(df$ss_file))
  # if (fe > 0) {
  #   print(paste0("ifold: ", ifold, ", ss missing: ", fe))
  # }  
  fe = sum(!file.exists(df$ss_robust_file))
  if (fe > 0) {
    print(paste0("ifold: ", ifold, ", ss rob missing: ", fe))
  }    
}
