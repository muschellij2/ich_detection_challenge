library(dplyr)
library(neurobase)
library(ggplot2)
library(tidyr)
setwd(here::here())

source("code/file_exists.R")

n_folds = 200


results = vector(mode = "list", length = n_folds)

full_outfile = file.path(
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
readr::write_rds(results, path = full_outfile)

df = readr::read_rds("wide_headers_with_folds_outcomes.rds")

results$have_results = TRUE
results = results %>% 
  left_join(df)
stopifnot(all(results$have_results))
results$have_results = NULL

results = results %>% 
  arrange(PatientID, StudyInstanceUID, SeriesInstanceUID, x, y, z) 

readr::write_rds(results, path = full_outfile)

train = results %>% 
  filter(group == "train")
# xx = train[ is.na(train$mean) &  train$any > 0,]
# x = train[ train$scan_id %in% xx$scan_id[1], ]
# img = readnii(x$outfile[1])
# img = window_img(img)
# mask = readnii(sub(".nii", "_Mask.nii", x$ss_robust_file[1]))
# ss = readnii(x$ss_robust_file[1])
# ss = window_img(ss)

tt = train %>% 
  select(
    scan_id, instance_number,
    `0%`, `25%`, `50%`, `75%`, `95%`, 
    `99%`, `100%`, `mean`, `median`, `sd`,
    any, epidural, intraparenchymal, 
    intraventricular, subarachnoid, subdural) %>% 
  tidyr::gather(statistic, value,
         `0%`, `25%`, `50%`, `75%`, `95%`, 
         `99%`, `100%`, `mean`, `median`, `sd`) %>% 
  tidyr::gather(outcome_measure, outcome,
                any, epidural, intraparenchymal, 
                intraventricular, subarachnoid, subdural)  
res = tt %>% 
  filter(!is.na(value)) %>% 
  group_by(statistic, outcome_measure, outcome) %>% 
  summarise(mean = mean(value, na.rm = TRUE),
            q0 = quantile(value, probs = 0, na.rm = TRUE),
            q25 = quantile(value, probs = 0.25, na.rm = TRUE),
            q50 = quantile(value, probs = 0.5, na.rm = TRUE),
            q75 = quantile(value, probs = 0.75, na.rm = TRUE),
            q100 = quantile(value, probs = 1, na.rm = TRUE),
  ) %>% 
  spread(outcome, mean)
  
tt %>% 
  ggplot(aes(y = value, x = factor(outcome))) + geom_boxplot() + 
  facet_wrap(~ outcome_measure + statistic)

