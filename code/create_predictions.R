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

scan_prev = train %>% 
  group_by(scan_id, outcome) %>% 
  summarise(Label = any(Label > 0)) %>% 
  group_by(outcome) %>% 
  summarise(prevalence = mean(Label))

prev = train %>% 
  group_by(outcome) %>% 
  summarise(prevalence = mean(Label))


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
write_test(test, path = file.path("predictions", "all_zero.csv"))

test = test %>% 
  mutate(Label = 1)
write_test(test, path = file.path("predictions", "all_ones.csv"))

test = test_df %>% 
  select(-Label) %>% 
  left_join(prev) %>% 
  rename(Label = prevalence) %>% 
  unite(ID, outcome, col = ID, sep = "_")
write_test(test, path = file.path("predictions", "prevalence.csv"))


test = test_df %>% 
  select(-Label) %>% 
  left_join(scan_prev) %>% 
  rename(Label = prevalence)  %>% 
  unite(ID, outcome, col = ID, sep = "_")
write_test(test, path = file.path("predictions", "scan_prevalence.csv"))


