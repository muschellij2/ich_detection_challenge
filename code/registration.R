# library(tidyverse)
# library(readr)
library(dplyr)
library(dcm2niir)
library(tibble)
library(tidyr)
library(neurobase)
library(extrantsr)
library(ichseg)
setwd(here::here())

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

df = readr::read_rds("wide_headers_with_folds.rds")
all_df = df

df = all_df
n_folds = 200

ifold = as.numeric(Sys.getenv("SGE_TASK_ID"))
if (is.na(ifold)) {
  ifold = 1
}

df = df %>% 
  filter(fold == ifold) %>% 
  select(fold, index, scan_id, outfile, 
         reg_file,
         reg_mat,
         ss_file, ss_robust_file) %>% 
  distinct()


uids = unique(df$index)
iid = uids[1]


for (iid in uids) {
  
  print(iid)
  run_df = df[ df$index == iid, ]
  
  template = "templates/scct_unsmooth_SS_0.01_128x128x128.nii.gz"
  
  infile = run_df$ss_file
  outfile = run_df$reg_file
  omat = run_df$reg_mat
  if (file.exists(infile)) {
    if (!all(file.exists(c(omat, outfile)))) {
      out = reg_func(infile, outfile, omat, template)
    }
  }
}
