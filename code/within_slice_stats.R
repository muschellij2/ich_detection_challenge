rm(list = ls())
library(RNifti)
library(oro.nifti)
library(neurobase)
library(dplyr)
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


ifold = as.numeric(Sys.getenv("SGE_TASK_ID"))
if (is.na(ifold)) {
  ifold = 114
}

df = df %>%
  filter(fold == ifold)

uids = unique(df$index)
iid = 16
results = vector(mode = "list", length = length(uids))
fold_outfile = file.path(
  "stats", 
  paste0("fold_", ifold, ".rds"))
for (i in seq_along(uids)) {
  iid = uids[i]
  print(iid)
  run_df = df[ df$index == iid, ]
  scan_id = unique(run_df$scan_id)
  run_df = run_df %>% 
    arrange(instance_number)
  outfile = unique(run_df$outfile)
  out_rds = file.path("stats", 
                      paste0(nii.stub(outfile, bn = TRUE), ".rds"))
  if (!file.exists(out_rds)) {
    ss_robust_file = unique(run_df$ss_robust_file)
    maskfile = sub("[.]nii", "_Mask.nii", ss_robust_file)
    stopifnot(file.exists(maskfile))
    
    pred_file = unique(run_df$predfile)
    prob_file = sub(".nii", "_prob.nii", pred_file)
    
    ss = RNifti::readNifti(ss_robust_file)
    mask = RNifti::readNifti(maskfile)
    ss[ mask == 0] = NA
    
    stats = function(x) {
      x = c(x)
      df = as.data.frame(t(
        quantile(x, na.rm = TRUE,
                 probs = c(0, 0.25, 0.5, 0.75, 0.95, 0.99, 1)
        )))
      df$mean = mean(x, na.rm = TRUE)
      df$median = median(x, na.rm = TRUE)
      df$sd = sd(x, na.rm = TRUE)
      df
    }
    res = apply(ss, 3, stats)
    names(res) = seq(dim(ss)[3])
    res = bind_rows(res, .id = "instance_number")
    res$scan_id = scan_id
    message("Writing the file")
    readr::write_rds(res, out_rds)
  } else {
    res = readr::read_rds(out_rds)
  }
  results[[i]] = res
}
results = dplyr::bind_rows(results)
readr::write_rds(results, path = fold_outfile)
