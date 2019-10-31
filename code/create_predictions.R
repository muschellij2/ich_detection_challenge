rm(list = ls())
library(neurobase)
library(dplyr)
library(tidyr)
setwd(here::here())
source("code/file_exists.R")
df = readr::read_rds("wide_headers_with_folds_outcomes.rds")

write_test = function(x, ...) {
  stopifnot(nrow(x) == 471270)
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
    paste0("rf_test_", ioutcome, "_", 
           num.trees,
           ".rds"))
  results[[ioutcome]] = readr::read_rds(path = test_outfile)
}
results = bind_rows(results)
wide = tidyr::spread(results, outcome, Label)
stopifnot(!any(is.na(wide)))
xres = results
results = results %>% 
  unite(ID, outcome, col = "ID", sep = "_") %>% 
  arrange(ID)
results = left_join(test_id_outcome, results)
results$Label[is.na(results$Label)] = 0
write_test(results, 
           path = file.path(
             "predictions", 
             paste0("rf_model_", num.trees, ".csv.gz")))

results$Label = as.numeric(results$Label > 0.5)
write_test(results, 
           path = file.path(
             "predictions", 
             paste0("rf_model_", num.trees, "_thresh.csv.gz")))

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
               paste0("rf_model_", num.trees, "_", 
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
             paste0("rf_model_", num.trees, "_prevalence.csv.gz")))

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
write_test(test, path = file.path("predictions", "all_zero.csv.gz"))

test = test %>% 
  mutate(Label = 1)
write_test(test, path = file.path("predictions", "all_ones.csv.gz"))

test = test_df %>% 
  select(-Label) %>% 
  left_join(prev) %>% 
  rename(Label = prevalence) %>% 
  unite(ID, outcome, col = ID, sep = "_")
write_test(test, path = file.path("predictions", "prevalence.csv.gz"))


test = test_df %>% 
  select(-Label) %>% 
  left_join(scan_prev) %>% 
  rename(Label = prevalence)  %>% 
  unite(ID, outcome, col = ID, sep = "_")
write_test(test, path = file.path("predictions", "scan_prevalence.csv.gz"))


