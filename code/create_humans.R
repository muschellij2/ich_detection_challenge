# Create human mask
rm(list = ls())
library(neurobase)
library(lungct)
library(ichseg)
library(dplyr)
library(fslr)
library(extrantsr)
setwd(here::here())

n_folds = 200
d3 = function(x) {
  x = fslval(x, keyword = "pixdim3", verbose = FALSE)
  as.numeric(x)
}

df = readr::read_rds("wide_headers_with_folds.rds")
all_df = df

df = all_df
df = df %>% 
  select(outfile, index, scan_id, fold) %>% 
  distinct()
# 7646
# ID_02c48e85-ID_bd2131d216 
ifold = as.numeric(Sys.getenv("SGE_TASK_ID"))
if (is.na(ifold)) {
  ifold = 51
}



df = df[ df$fold == ifold,]

uids = unique(df$index)
iid = uids[1]

for (iid in uids) {
  
  print(iid)
  run_df = df[ df$index == iid, ]
  outfile = unique(run_df$outfile)
  out_mask = file.path("human", basename(outfile))
  
  if (!file.exists(out_mask)) {
    res = lungct::segment_human(outfile, verbose = TRUE)
    write_nifti(res$body, out_mask)
  } else {
    vd = d3(outfile)
    vd_mask = d3(out_mask)
    if (vd != vd_mask) {
      print("bad")
      file.remove(out_mask)
    }
  }

  
}


