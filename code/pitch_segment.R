rm(list = ls())
library(neurobase)
library(ichseg)
library(dplyr)
setwd(here::here())


tmp = sapply(c("ss", "mask", "nifti"), dir.create, 
             showWarnings = FALSE)

n_folds = 200

df = readr::read_rds("wide_headers_with_folds.rds")
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
# iid = 15977
# iid =102
#iid = 219
for (iid in uids) {
  
  print(iid)
  run_df = df[ df$index == iid, ]
  
  ss_robust_file = unique(run_df$ss_robust_file)
  outfile = unique(run_df$outfile)
  pred_file = unique(run_df$predfile)
  prob_file = sub(".nii", "_prob.nii", pred_file)
  
  if (!file.exists(pred_file)) {
    
    res = ich_segment(
      img = outfile,
      mask = ss_robust_file,
      model = "rf", 
      native = TRUE,
      outfile = pred_file)
    img = readnii(outfile)
    native_res = res$native_prediction
    rm(res)
    native_prob = native_res$smoothed_probability_image 
    writenii(native_prob, prob_file)
    
  }
  
}
