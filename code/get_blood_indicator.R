rm(list = ls())
library(dcm2niir)
library(divest)
library(oro.nifti)
library(neurobase)
library(extrantsr)
library(dplyr)
library(fs)
library(dcmtk)
library(fslr)
setwd(here::here())

source("code/file_exists.R")
add_instance_number = TRUE


tmp = sapply(c("ss", "mask", "nifti"), dir.create, 
             showWarnings = FALSE)

n_folds = 200

df = readr::read_rds("wide_headers_with_folds_outcomes.rds")
all_df = df

df = all_df

ddf = df %>% 
  select(scan_id, fold, index, predfile, outfile, maskfile) %>% 
  distinct()

outcomes = c("any", "epidural", "intraparenchymal", "intraventricular", 
             "subarachnoid", "subdural")
positive_scans = df %>% 
  filter(any > 0) %>% 
  select(scan_id) %>% 
  distinct() %>% 
  pull(scan_id)
positives = df %>% 
  filter(scan_id %in% positive_scans)
# x = positives[ positives$scan_id == positives$scan_id[1],]

n_folds = 200


# ifold = as.numeric(Sys.getenv("SGE_TASK_ID"))
# if (is.na(ifold)) {
#   ifold = 114
# }
# 
# df = df %>% 
#   filter(fold == ifold)

uids = unique(df$index)
iid = 16

for (iid in uids) {
  
  print(iid)
  run_df = df[ df$index == iid, ]
  run_df = run_df %>% 
    arrange(instance_number)

  ss_robust_file = unique(run_df$ss_robust_file)
  maskfile = sub("[.]nii", "_Mask.nii", ss_robust_file)
  stopifnot(file.exists(maskfile))
  outfile = unique(run_df$outfile)
  pred_file = unique(run_df$predfile)
  prob_file = sub(".nii", "_prob.nii", pred_file)
  
  ss = readnii(ss_robust_file)
  prob = readnii(prob_file)
  pred = readnii(pred_file)
  thresh = 0.1
  coarse_pred = prob > thresh
  

  blood_pred = apply(pred, 3, sum)
  blood_pred_vol = voxres(pred, units = "cm") * blood_pred
  blood = apply(coarse_pred, 3, sum)
  blood_vol = voxres(pred, units = "cm") * blood
  
  k = round(1/voxres(prob, units = "cm"))
  res = ants_bwlabel(img = coarse_pred, k = k)
  coarse_blood = apply(res, 3, sum)
  coarse_blood_vol = voxres(pred, units = "cm") * coarse_blood
  
  prediction_cc = ants_bwlabel(img = pred, k = k)
  prediction_cc_blood = apply(prediction_cc, 3, sum)
  
  
}
