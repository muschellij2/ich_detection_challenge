# library(tidyverse)
# library(readr)
library(dplyr)
library(neurobase)
library(extrantsr)
setwd(here::here())

source("code/file_exists.R")

stage_number = 2
pre = ifelse(stage_number == 1, "", "stage2_")

reg_func = function(infile, outfile, omat, template) {
  ss = registration(
    filename = infile,
    outfile = outfile,
    template.file = template,
    other_interpolator = "NearestNeighbor",
    typeofTransform = "Rigid",
    retimg = FALSE,
    interpolator = "NearestNeighbor")
  file.copy(
    ss$fwdtransforms,
    omat,
    overwrite = TRUE
  )
  return(ss)
}

df = readr::read_rds(paste0(pre, "wide_headers_with_folds.rds"))
all_df = df

ddf = df %>% 
  select(scan_id, fold, index, predfile, outfile, maskfile) %>% 
  distinct()

df = all_df
n_folds = 200


df = df %>% 
  select(fold, index, scan_id, outfile, maskfile,
         reg_file,
         reg_mat,
         ss_file, ss_robust_file) %>% 
  distinct()

ifold = as.numeric(Sys.getenv("SGE_TASK_ID"))
if (is.na(ifold)) {
  ifold = 114
}

df = df %>% 
  filter(fold == ifold)

uids = unique(df$index)
iid = uids[1]


for (iid in uids) {
  
  print(iid)
  run_df = df[ df$index == iid, ]
  
  template = "templates/scct_unsmooth_SS_0.01_128x128x128.nii.gz"
  
  infile = run_df$ss_file
  outfile = run_df$reg_file
  maskfile = run_df$maskfile
  omat = run_df$reg_mat
  # img = readnii(infile)
  # mask = readnii(maskfile)
  # masked = mask_img(img + 1024, mask)
  # bc = bias_correct(masked, mask = mask, correction="N4")
  # bci = mask_img(bc, mask) - 1024
  if (file.exists(infile)) {
    if (!all(file.exists(c(omat, outfile)))) {
      out = reg_func(infile, outfile, omat, template)
    }
  }
  
  infile = sub("ss/", "ss_robust/", run_df$ss_file)
  outfile = sub("reg/", "reg_robust/", run_df$reg_file)
  maskfile = sub("ss/", "ss_robust/", run_df$maskfile)
  omat = sub("reg/", "reg_robust/", run_df$reg_mat)
  if (file.exists(infile)) {
    if (!all(file.exists(c(omat, outfile)))) {
      out = reg_func(infile, outfile, omat, template)
    }
  }
  
}
