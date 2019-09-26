rm(list = ls())
library(dcm2niir)
library(neurobase)
library(extrantsr)
library(ichseg)
library(tibble)
library(dplyr)
library(fs)
library(lungct)
setwd(here::here())

on_cluster = function() {
  Sys.info()[["user"]] == "jmuschel"
}
check_gz = !on_cluster()
df = readr::read_rds("stage_1_train_data.rds")

ifold = as.numeric(Sys.getenv("SGE_TASK_ID"))
if (is.na(ifold)) {
  ifold = 1
}
# all(df$ID %in% labs$ID)
# setdiff(df$ID, labs$ID)
df = df[ df$fold == ifold,]

if (on_cluster()) {
  run_df = df %>%
    filter(file.exists(dcm))
} else {
  run_df = df
}

iid = 1
for (iid in seq(nrow(run_df))) {
  print(iid)
  dcm = run_df$dcm[iid]
  outfile = run_df$nifti[iid]
  body_outfile = run_df$body[iid]
  pngname = run_df$png[iid]
  if (!file.exists(outfile)) {
    res = ct_dcm2nii(files = dcm, drop_dim = FALSE,
                     opts = paste0(
                       "-9 ",
                       " -v y ",
                       " -z i -f %p_%t_%s"))
    print(class(res))
    writenii(res, outfile, drop_dim = FALSE)
  } else {
    res = readnii(outfile, drop_dim = FALSE)
  }
  
  if (!file.exists(body_outfile)) {
    x = lungct::segment_human(res, drop_dim = FALSE,
                              smooth = FALSE,
                              lthresh = -400)
    write_nifti(x$body, body_outfile)
    body = check_nifti(x$body, drop_dim = FALSE)
  } else {
    body = readnii(body_outfile, drop_dim = FALSE)
  }
  
  if (!file.exists(pngname)) {
    png(pngname, res = 300, units = "in", height = 7, width = 7)
    ortho2(res, body)
    dev.off()
  }
}

