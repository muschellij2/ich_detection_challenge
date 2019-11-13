library(dplyr)
library(neurobase)
library(ggplot2)
library(tidyr)
library(ranger)
setwd(here::here())
source("code/file_exists.R")

stage_number = 2
pre = ifelse(stage_number == 1, "", "stage2_")

nthreads = 1
full_df = readr::read_rds(paste0(pre, 
                                 "wide_headers_with_folds_outcomes.rds"))
full_df = full_df %>% 
  select(scan_id, group, outfile) %>% 
  distinct()
all_scan_ids = unique(full_df$scan_id)
full_data = split(full_df, full_df$group)
rm(full_df)

hdr = readr::read_rds(paste0(pre, "wide_headers.rds"))
cn = colnames(hdr)
cn = setdiff(cn, c("file", "ID", "x", "y", "z"))
rm(hdr)

full_outfile = file.path(
  "stats", 
  paste0(pre, "all_data.rds"))
results = readr::read_rds(path = full_outfile)
stopifnot(all( all_scan_ids %in% unique(results$scan_id)))
df = results %>% 
  group_by(scan_id) %>% 
  mutate(z_mean = z - mean(z)) %>% 
  ungroup()

df = df[ , !colnames(df) %in% cn]
rm(results)

df = df %>% 
  rename(quant_0 = `0%`,
         quant_25 = `25%`,
         quant_50 = `50%`,
         quant_75 = `75%`,
         quant_95 = `95%`,
         quant_99 = `99%`,
         quant_100 = `100%`)
colnames(df) = sub("%", "_quant", colnames(df))

df = df %>% 
  mutate(sd = ifelse(n_voxels == 1, 0, sd),
         pitch_sd  = ifelse(n_voxels == 1, 0, pitch_sd),
         prob_sd  = ifelse(n_voxels == 1, 0, prob_sd)
  )
tab = table(df$group)
n_train = tab["train"]
n_test = tab["test"]

df = df %>% 
  select(
    group,
    scan_id,
    ID,
    instance_number, any, epidural, 
    intraparenchymal, intraventricular, 
    subarachnoid, subdural, 
    n_voxels,
    contains("quant"), matches("mean|sd|median"), 
    matches("sum|pct"))

training = df %>% 
  filter(group == "train") %>% 
  select(-group)

xtraining = training

testing = df %>% 
  filter(group == "test") %>% 
  select(-group)

rm(df)
training = training %>% 
  filter(n_voxels > 0) 
stopifnot(all( full_data$train$scan_id %in% unique(training$scan_id)))

outcomes = c("any", "epidural", "intraparenchymal", "intraventricular", 
             "subarachnoid", "subdural")
tree_options = c(500, 2000)
eg = expand.grid(outcome = outcomes, 
                 num.trees = tree_options,
                 stringsAsFactors = FALSE)

iscen = as.numeric(Sys.getenv("SGE_TASK_ID"))
if (is.na(iscen)) {
  iscen = 7
}


print(iscen)
ieg = eg[iscen,]
ioutcome = ieg$outcome
num.trees = ieg$num.trees



outfile = file.path(
  "predictions", 
  paste0("rf_", ioutcome, "_", num.trees, ".rds"))
if (!file.exists(outfile)) {
  X = training %>% 
    select(-scan_id, 
           -ID, 
           -instance_number,
           -any, -epidural, 
           -intraparenchymal, -intraventricular, 
           -subarachnoid, -subdural,
           -contains("median"),
           one_of(ioutcome))
  cn = colnames(X)
  cn[ cn == ioutcome] = "y"
  colnames(X) = cn
  
  
  
  X = X %>% 
    select(-one_of(outcomes))
  rm(training)
  stopifnot(!any(is.na(X)))
  
  X = X %>% 
    as.data.frame
  X$y = factor(X$y)
  
  mod = ranger(y ~ ., data = X, 
               num.trees = num.trees,
               num.threads = nthreads,
               probability = TRUE)
  attr(mod, "outcome_used") = ioutcome
  readr::write_rds(mod, 
                   path = outfile)
  rm(X)
} else {
  mod = readr::read_rds(outfile)
}
print(mod)
# mod = readRDS( file.path("predictions", "quick_20_trees.rds"))

testing = testing %>% 
  select(-one_of(outcomes))

stopifnot(all( full_data$test$scan_id %in% unique(testing$scan_id)))


test_outfile = file.path(
  "predictions",
  paste0("rf_test_", ioutcome, "_", 
         num.trees,
         ".rds"))
if (!file.exists(test_outfile)) {
  out = rep(NA, nrow(testing))
  index = testing$n_voxels > 0
  stopifnot(!any(is.na(testing[index,])))
  
  pred = predict(mod, data = testing[index, ],
                 num.threads = nthreads)
  pred = pred$predictions[, "1"]
  
  out[index] = pred
  out[is.na(out)] = 0
  testing$Label = out
  out = testing %>% 
    select(ID, Label) %>% 
    mutate(outcome = ioutcome)
  readr::write_rds(out, path = test_outfile)
}

train_outfile = file.path(
  "predictions",
  paste0("rf_train_", ioutcome, "_", 
         num.trees,
         ".rds"))
training = xtraining
# rm(xtraining)
if (!file.exists(train_outfile)) {
  stopifnot(nrow(training) == n_train)
  out = rep(NA, nrow(training))
  index = training$n_voxels > 0
  stopifnot(!any(is.na(training[index,])))
  
  pred = predict(mod, data = training[index, ],
                 num.threads = nthreads)
  pred = pred$predictions[, "1"]
  
  out[index] = pred
  out[is.na(out)] = 0
  training$Label = out
  out = training %>% 
    select(ID, Label) %>% 
    mutate(outcome = ioutcome)
  readr::write_rds(out, path = train_outfile)
}
