rm(list = ls())
library(RNifti)
library(oro.nifti)
library(neurobase)
library(dplyr)
library(fslr)
setwd(here::here())

source("code/file_exists.R")
add_instance_number = TRUE

stage_number = 2
pre = ifelse(stage_number == 1, "", "stage2_")

tmp = sapply(c("ss", "mask", "nifti"), dir.create, 
             showWarnings = FALSE)

n_folds = 200

df = readr::read_rds(paste0(pre, "wide_headers_with_folds.rds"))

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
i = 1

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
    
    prob = RNifti::readNifti(prob_file)
    ss = RNifti::readNifti(ss_robust_file)
    mask = RNifti::readNifti(maskfile)
    ss[ mask == 0] = NA
    prob[ mask == 0] = NA
    
    
    stats = function(x, run_pct = TRUE, run_median = TRUE) {
      x = c(x)
      df = as.data.frame(t(
        quantile(x, na.rm = TRUE,
                 probs = c(0, 0.25, 0.5, 0.75, 0.95, 0.99, 1)
        )))
      df$mean = mean(x, na.rm = TRUE)
      if (run_pct) {
        df$pct_30_80 = mean(x > 30 & x < 80, na.rm = TRUE)
        df$pct_40_80 = mean(x > 40 & x < 80, na.rm = TRUE)
        df$pct_40_60 = mean(x > 40 & x < 60, na.rm = TRUE)
        df$sum_30_80 = sum(x > 30 & x < 80, na.rm = TRUE)
        df$sum_40_80 = sum(x > 40 & x < 80, na.rm = TRUE)
        df$sum_40_60 = sum(x > 40 & x < 60, na.rm = TRUE)      
      }
      if (run_median) {
        df$median = median(x, na.rm = TRUE)
      }
      df$sd = sd(x, na.rm = TRUE)
      df
    }
    res = apply(ss, 3, stats)
    names(res) = seq(dim(ss)[3])
    res = bind_rows(res, .id = "instance_number")
    res$scan_id = scan_id
    
    
    prob_res = apply(prob > 0.1, 3, stats, 
                     run_pct = FALSE, run_median = FALSE)
    names(prob_res) = seq(dim(ss)[3])
    prob_res = bind_rows(prob_res, .id = "instance_number")
    cn = colnames(prob_res)
    cn = cn[ !grepl("pct|sum", cn)]
    prob_res = prob_res[, cn]
    cn = paste0("pitch_", cn)
    cn[ cn == "pitch_instance_number"] = "instance_number"
    colnames(prob_res) = cn
    prob_res$scan_id = scan_id
    res = left_join(res, prob_res)
    
    prob_res = apply(prob, 3, stats, 
                     run_pct = FALSE, run_median = TRUE)
    names(prob_res) = seq(dim(ss)[3])
    prob_res = bind_rows(prob_res, .id = "instance_number")
    cn = colnames(prob_res)
    cn = cn[ !grepl("pct|sum", cn)]
    prob_res = prob_res[, cn]
    cn = paste0("prob_", cn)
    cn[ cn == "prob_instance_number"] = "instance_number"
    colnames(prob_res) = cn
    prob_res$scan_id = scan_id  
    
    res = left_join(res, prob_res)
    res$instance_number = as.integer(res$instance_number)
    
    n_voxels = apply(mask, 3, sum)
    n_voxels = data.frame(n_voxels = n_voxels, 
                          instance_number = seq(dim(ss)[3]), 
                          stringsAsFactors = FALSE)
    res = left_join(res, n_voxels)
    
    message("Writing the file")
    readr::write_rds(res, out_rds)
    
  } else {
    res = readr::read_rds(out_rds)
  }
  results[[i]] = res
}
results = dplyr::bind_rows(results)
readr::write_rds(results, path = fold_outfile)
