# Create human mask
rm(list = ls())
library(ANTsRCore)
library(neurobase)
library(lungct)
library(ichseg)
library(dplyr)
library(fslr)
library(extrantsr)
setwd(here::here())
# Rcpp::sourceCpp("code/dist_min.cpp")

hausdorffDistance <- function( binarySeg1, binarySeg2 ) {
  binarySeg1 = check_ants(binarySeg1)
  binarySeg2 = check_ants(binarySeg2)
  d1 = iMath( binarySeg1, "MaurerDistance" ) * binarySeg2
  d2 = iMath( binarySeg2, "MaurerDistance" ) * binarySeg1
  return( max( c( max( abs( d1 ) ), max( abs( d2 ) ) ) ) )
}

stage_number = 2
pre = ifelse(stage_number == 1, "", "stage2_")

n_folds = 200

df = readr::read_rds(paste0(pre, "wide_headers_with_folds.rds"))

# all_df = df

# df = all_df
df = df %>% 
  select(outfile, index, scan_id, fold, maskfile, ss_file) %>% 
  mutate(dist_file = file.path("dist", basename(outfile))) %>% 
  distinct()
# 7646
# ID_02c48e85-ID_bd2131d216 
ifold = as.numeric(Sys.getenv("SGE_TASK_ID"))
if (is.na(ifold)) {
  ifold = 155
}

df = df[ df$fold == ifold,]

uids = unique(df$index)
iid = uids[1]

for (iid in uids) {
  
  
  print(iid)
  run_df = df[ df$index == iid, ]
  outfile = unique(run_df$outfile)
  ofile = run_df$dist_file[1]
  
  if (!file.exists(ofile)) {
    
    ss_file = unique(run_df$ss_file)
    maskfile = unique(run_df$maskfile)
    out_maskfile = sub("[.]nii", "_Mask.nii", ss_file)
    
    
    fill_size = 5
    filled = filler(out_maskfile, fill_size = fill_size)
    res = oMath(filled, "MaurerDistance")
    mask = readnii(out_maskfile)
    result = mask_img(res * -1, mask)
    
    write_nifti(result, ofile)
  }
  
  # 
  # ero = filler(filled, fill_size = 1, dilate = FALSE)
  # surf = filled - ero
  # 
  # rm(ero)
  # 
  # vdim = voxdim(surf)
  # all_ind = t(which(filled > 0, arr.ind = TRUE))
  # all_ind = all_ind * vdim
  # surf_ind = t(which(surf > 0, arr.ind = TRUE))
  # surf_ind = surf_ind * vdim
  # 
  # rm(surf)
  # # rm(filled)
  # gc()
  # 
  # # all_ind = matrix(rnorm(3e5*3), nrow = 3)
  # # surf_ind = matrix(rnorm(1e4*3), nrow = 3)
  # 
  # s2 = colSums(surf_ind^2)
  # y2 = colSums(all_ind^2)
  # 
  # # 12gb
  # n_gb = 2
  # n_gb = n_gb * 1024^3
  # chunk_size = ceiling(n_gb / 8 / ncol(surf_ind))
  # chunks = rep(1:ceiling(ncol(all_ind)/chunk_size), each = chunk_size)
  # chunks = chunks[1:ncol(all_ind)]
  # d = rep(NA, length = ncol(all_ind))
  # ichunk = 1
  # for (ichunk in 1:chunk_size) {
  #   print(ichunk)
  #   ind = which(chunks == ichunk)
  #   x = t(all_ind[,ind])
  #   yy = y2[ind]
  #   # -2xy
  #   xy = -2 * (x %*% surf_ind)
  #   # y^2 - 2xy
  #   xy = xy + yy
  #   # y^2 - 2xy + x^2
  #   xy = t(xy) + s2
  #   res = matrixStats::colMins(xy)
  #   rm(xy)
  #   res = round(res, digits = 5)
  #   d[ind] = res
  #   rm(ind);
  # }
  # dimg = remake_img(vec = d, img = filled, mask = filled)
  # 
}


