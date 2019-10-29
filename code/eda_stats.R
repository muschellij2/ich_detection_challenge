library(dplyr)
library(neurobase)
library(ggplot2)
library(tidyr)
library(ranger)
setwd(here::here())

source("code/file_exists.R")

n_folds = 200

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
  iioutcome = 1
}

ioutcome = outcomes[iioutcome]


training = training
rm(training)


tt = train %>%
  select(
    scan_id, instance_number,
    `0%`, `25%`, `50%`, `75%`, `95%`,
    `99%`, `100%`, `mean`, `median`, `sd`,
    pct_30_80, pct_40_80, pct_40_60,
    sum_30_80, sum_40_80, sum_40_60,
    any, epidural, intraparenchymal,
    intraventricular, subarachnoid, subdural) %>%
  tidyr::gather(statistic, value,
                `0%`, `25%`, `50%`, `75%`, `95%`,
                `99%`, `100%`, `mean`, `median`, `sd`,
                pct_30_80, pct_40_80, pct_40_60,
                sum_30_80, sum_40_80, sum_40_60) %>%
  tidyr::gather(outcome_measure, outcome,
                any, epidural, intraparenchymal,
                intraventricular, subarachnoid, subdural)

tt %>%
  filter(statistic %in% c("mean", "median", "sd")) %>%
  filter(!is.na(value)) %>%
  ggplot(aes(y = value, x = outcome_measure,
             colour = factor(outcome))) + geom_boxplot() +
  facet_wrap(~ statistic)

tt %>%
  filter(statistic == "mean") %>%
  filter(!is.na(value)) %>%
  ggplot(aes(x = value, colour = factor(outcome))) +
  geom_line(stat = "density") +
  xlim(c(-50, 50)) +
  facet_wrap(~ outcome_measure)


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



library(dplyr)
library(neurobase)
library(ggplot2)
library(tidyr)
setwd(here::here())

source("code/file_exists.R")

n_folds = 200

full_outfile = file.path(
  "stats",
  paste0("all_data.rds"))
results = readr::read_rds(path = full_outfile)





