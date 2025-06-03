rm(list = ls())
library(neurobase)
library(dplyr)
library(tidyr)
setwd(here::here())
source("code/file_exists.R")

stage_number = 2
pre = ifelse(stage_number == 1, "", "stage2_")

ss = readr::read_csv(
  paste0("stage_", stage_number, "_sample_submission.csv.gz")) %>% 
  arrange(ID)
test_ids = ss %>% 
  separate(ID, into = c("ID", "ID2", "outcome")) %>% 
  unite(col = ID, ID, ID2, sep = "_") %>% 
  select(ID) %>% 
  distinct() %>% 
  pull()

n_needed = nrow(ss)
df = readr::read_rds(
  paste0(pre,"wide_headers_with_folds_outcomes.rds"))

write_test = function(x, ...) {
  # stopifnot(nrow(x) == 471270)
  stopifnot(nrow(x) == n_needed)
  stopifnot(all(colnames(x) == c("ID", "Label")))
  stopifnot(!any(is.na(x$Label)))
  x$Label[ x$Label > 1 ] = 1
  x$Label[ x$Label < 0 ] = 0
  readr::write_csv(x, ...)
}

train = df %>% 
  filter(group == "train") %>% 
  select(
    scan_id,
    ID,
    any,
    epidural,
    intraparenchymal,
    intraventricular,
    subarachnoid,
    subdural) %>% 
  gather(outcome, value = Label, -ID, -scan_id)
test_id_outcome = df %>% 
  filter(group == "test") %>% 
  filter(ID %in% test_ids) 
stopifnot(all(test_id_outcome$ID %in% test_ids))
stopifnot(all(test_ids %in% test_id_outcome$ID))
test_id_outcome = test_id_outcome %>% 
  select(
    ID,
    any,
    epidural,
    intraparenchymal,
    intraventricular,
    subarachnoid,
    subdural) %>%   
  gather(outcome, value = Label, -ID) %>% 
  unite(ID, outcome, col = "ID", sep = "_") %>% 
  arrange(ID) %>% 
  select(ID)
stopifnot(all(test_id_outcome$ID %in% ss$ID))
stopifnot(all(ss$ID %in% test_id_outcome$ID))

scan_prev = train %>% 
  group_by(scan_id, outcome) %>% 
  summarise(Label = any(Label > 0)) %>% 
  group_by(outcome) %>% 
  summarise(prevalence = mean(Label))

prev = train %>% 
  group_by(outcome) %>% 
  summarise(prevalence = mean(Label))

##########################################
# Adding in RF Model
##########################################
outcomes = c("any", "epidural", "intraparenchymal", "intraventricular", 
             "subarachnoid", "subdural")
num.trees = 2000
results = vector(mode = "list", length = length(outcomes))
names(results) = outcomes
ioutcome = outcomes[1]
for (ioutcome in outcomes) {
  print(ioutcome)
  test_outfile = file.path(
    "predictions",
    paste0(pre, "rf_test_", ioutcome, "_", 
           num.trees,
           ".rds"))
  results[[ioutcome]] = readr::read_rds(path = test_outfile)
}
results = bind_rows(results)
results = results[ results$ID %in% test_ids, ]
wide = tidyr::spread(results, outcome, Label)
stopifnot(!any(is.na(wide)))
stopifnot(all(wide$ID %in% test_ids))
xres = results
results = results %>% 
  unite(ID, outcome, col = "ID", sep = "_") %>% 
  arrange(ID)
results = left_join(test_id_outcome, results)
results$Label[is.na(results$Label)] = 0
write_test(results, 
           path = file.path(
             "predictions", 
             paste0(pre, "rf_model_", num.trees, ".csv.gz")))

cp = readr::read_rds(paste0("results/cutpoints_test.rds"))
if (!is.data.frame(cp)) {
  cp = cp$cutpoints
}
results = xres %>% 
  left_join(cp[, c("outcome", "cutoff")])
results$Label = as.numeric(results$Label > results$cutoff)
results$cutoff = NULL
results = results %>% 
  unite(ID, outcome, col = "ID", sep = "_") %>% 
  arrange(ID)
results = left_join(test_id_outcome, results)
results$Label[is.na(results$Label)] = 0
write_test(results, 
           path = file.path(
             "predictions", 
             paste0(pre, "rf_model_", num.trees, "_cutoff.csv.gz")))


results = xres %>% 
  unite(ID, outcome, col = "ID", sep = "_") %>% 
  arrange(ID)
results = left_join(test_id_outcome, results)
results$Label[is.na(results$Label)] = 0
# cutoff = !!!
results$Label = as.numeric(results$Label > 0.5)
write_test(results, 
           path = file.path(
             "predictions", 
             paste0(pre, "rf_model_", num.trees, "_thresh.csv.gz")))

funcs = c("sum", "mean", "median", "max", "min")

for (ifunc in funcs) {
  print(ifunc)
  func = get(ifunc)
  results = xres %>% 
    filter(outcome != "any") %>% 
    arrange(ID)  
  any_res = results %>% 
    group_by(ID) %>% 
    summarize(Label = func(Label)) %>% 
    mutate(outcome = "any",
           Label = ifelse(Label > 1, 1, Label))
  results = bind_rows(results, any_res) %>% 
    arrange(ID) %>% 
    unite(ID, outcome, col = ID, sep = "_") 
  
  results = left_join(test_id_outcome, results)
  results$Label[is.na(results$Label)] = 0
  # threshold results > 0.5
  write_test(results, 
             path = file.path(
               "predictions", 
               paste0(pre, "rf_model_", num.trees, "_", 
                      ifunc, "Any.csv.gz")))
}

test = xres %>% 
  left_join(prev) %>% 
  mutate(Label = Label * prevalence) %>% 
  unite(ID, outcome, col = ID, sep = "_") %>% 
  select(-prevalence)
test = left_join(test_id_outcome, test)
test$Label[is.na(test$Label)] = 0
write_test(test, 
           path = file.path(
             "predictions", 
             paste0(pre, "rf_model_", num.trees, "_prevalence.csv.gz")))

test_df = df %>% 
  filter(group == "test") %>% 
  select(ID,
         any,
         epidural,
         intraparenchymal,
         intraventricular,
         subarachnoid,
         subdural) %>% 
  gather(outcome, value = Label, -ID)
test = test_df %>% 
  unite(ID, outcome, col = ID, sep = "_")
test = test %>% 
  mutate(Label = 0)
write_test(test, 
           path = file.path(
             "predictions", 
             paste0(pre, "all_zero.csv.gz")))

test = test %>% 
  mutate(Label = 1)
write_test(test, 
           path = file.path("predictions", 
                            paste0(pre, "all_ones.csv.gz")))

test = test_df %>% 
  select(-Label) %>% 
  left_join(prev) %>% 
  rename(Label = prevalence) %>% 
  unite(ID, outcome, col = ID, sep = "_")
write_test(test, 
           path = file.path("predictions", 
                            paste0(pre, "prevalence.csv.gz")))


test = test_df %>% 
  select(-Label) %>% 
  left_join(scan_prev) %>% 
  rename(Label = prevalence)  %>% 
  unite(ID, outcome, col = ID, sep = "_")
write_test(test, 
           path = file.path("predictions", 
                            paste0(pre, "scan_prevalence.csv.gz")))


