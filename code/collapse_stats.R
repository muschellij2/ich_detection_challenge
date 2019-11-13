library(dplyr)
library(neurobase)
library(ggplot2)
library(tidyr)
setwd(here::here())

source("code/file_exists.R")

n_folds = 200
stage_number = 2
pre = ifelse(stage_number == 1, "", "stage2_")

results = vector(mode = "list", length = n_folds)

outfile = file.path(
  "stats", 
  paste0("all_folds.rds"))
full_outfile = file.path(
  "stats", 
  paste0("all_data.rds"))
for (ifold in seq(n_folds)) {
  print(ifold)
  fold_outfile = file.path(
    "stats", 
    paste0("fold_", ifold, ".rds"))
  res = readr::read_rds(fold_outfile)
  results[[ifold]] = res
}

results = dplyr::bind_rows(results)
results = results %>% 
  mutate_at(vars(instance_number), as.numeric)
readr::write_rds(results, path = outfile)

df = readr::read_rds("wide_headers_with_folds_outcomes.rds")

results$have_results = TRUE
results = results %>% 
  left_join(df)
stopifnot(all(results$have_results))
results$have_results = NULL

results = results %>% 
  arrange(PatientID, StudyInstanceUID, SeriesInstanceUID, x, y, z) 

readr::write_rds(results, path = full_outfile)

