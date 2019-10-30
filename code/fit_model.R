library(dplyr)
library(neurobase)
library(ggplot2)
library(tidyr)
library(ranger)
setwd(here::here())

source("code/file_exists.R")

nthreads = 1

hdr = readr::read_rds("wide_headers.rds")
cn = colnames(hdr)
cn = setdiff(cn, c("file", "ID", "x", "y", "z"))
rm(hdr)

full_outfile = file.path(
  "stats", 
  paste0("all_data.rds"))
results = readr::read_rds(path = full_outfile)

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

testing = df %>% 
  filter(group == "test") %>% 
  select(-group)

rm(df)
training = training %>% 
  filter(n_voxels > 0) 

outcomes = c("any", "epidural", "intraparenchymal", "intraventricular", 
             "subarachnoid", "subdural")

iioutcome = as.numeric(Sys.getenv("SGE_TASK_ID"))
if (is.na(iioutcome)) {
  iioutcome = 2
}

ioutcome = outcomes[iioutcome]

outfile = file.path("predictions", 
                    paste0("rf_", ioutcome, ".rds"))
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
  if (!interactive()) {
    rm(training)
  }
  stopifnot(!any(is.na(X)))
  
  X = X %>% 
    as.data.frame
  X$y = factor(X$y)
  num.trees = 500
  
  mod = ranger(y ~ ., data = X, 
               num.trees = num.trees,
               num.threads = nthreads,
               probability = TRUE)
  attr(mod, "outcome_used") = ioutcome
  readr::write_rds(mod, 
                   path = outfile)
} else {
  mod = readr::read_rds(outfile)
}
mod
# mod = readRDS( file.path("predictions", "quick_20_trees.rds"))

test_outfile = file.path("predictions",
                         paste0("test_", ioutcome, ".rds"))
testing = testing %>% 
  select(-one_of(outcomes))
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
