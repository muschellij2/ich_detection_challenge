rm(list = ls())
library(readr)
library(dplyr)
library(RNifti)
library(neurobase)
setwd(here::here())


get_stats = function(file, maskfile) {
  img = RNifti::readNifti(file)
  mask = RNifti::readNifti(maskfile)
  ind = which(mask > 0, arr.ind = TRUE)
  vals = img[ind]
  ind = as.data.frame(ind)
  ind$vals = vals
  ind = ind %>% 
    rename(instance_number = dim3)
  stats = ind %>% 
    summarise(
      mn = mean(vals), 
      sd = sd(vals), 
      md = median(vals),
      mn_over_30 = mean(vals > 30 & vals < 80),
      mn_over_40 = mean(vals > 40 & vals < 80),
      mn_over_50 = mean(vals > 50 & vals < 80)
    )
  slice_stats = ind %>% 
    group_by(instance_number) %>% 
    summarise(
      n = n(),
      mn = mean(vals), 
      sd = sd(vals), 
      md = median(vals),
      mn_over_30 = mean(vals > 30 & vals < 80),
      mn_over_40 = mean(vals > 40 & vals < 80),
      mn_over_50 = mean(vals > 50 & vals < 80)
    )
  return(list(stats = stats, slice_stats = slice_stats))
}


df = readr::read_rds("wide_headers_with_folds_outcomes.rds")

all_df = df

df = all_df

# ID_02c48e85-ID_bd2131d216 
ifold = as.numeric(Sys.getenv("SGE_TASK_ID"))
if (is.na(ifold)) {
  ifold = 147
}

df = df[ df$fold == ifold,]

uids = unique(df$index)
iid = uids[1]

for (iid in uids) {
  
  print(iid)
  run_df = df[ df$index == iid, ]
  
  res = get_stats(run_df$outfile[1], run_df$maskfile[1])
  ss = res$slice_stats
  stats = res$stats
  
  xx = run_df %>% 
    left_join(ss)
  xx = xx %>% 
    select(n, mn, sd, md, starts_with("mn_over"), ID, file, 
           any, epidural, intraparenchymal, intraventricular, 
           subarachnoid, subdural) %>% 
    mutate_at(vars(n, mn, sd, md, starts_with("mn_over")), 
              tidyr::replace_na, 0)
  
  
}


