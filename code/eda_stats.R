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
