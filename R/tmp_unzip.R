library(dplyr)
z = readr::read_rds(here::here("original_data/zip_files.rds"))
z = z %>% 
  mutate(fold = floor(seq(nrow(z)) / n_folds) + 1)
