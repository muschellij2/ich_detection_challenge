rm(list=ls())
library(dcm2niir)
library(neurobase)
library(ANTsRCore)
library(extrantsr)
library(ichseg)
library(tibble)
library(dplyr)
library(fs)
setwd(here::here())

padded = FALSE
n_folds = 200

labs = readr::read_rds("stage_1_train.rds")
# dcms = list.files(pattern = ".dcm([.]gz|)$", 
#                   path = "stage_1_train_images",
#                   full.names = TRUE)
dcms = unique(paste0(labs$ID, ".dcm"))
dcms = file.path("stage_1_train_images", dcms)
df = tibble::tibble(
  dcm = dcms,
  nifti = path("nifti", basename(sub("[.]dcm([.]gz|)$", ".nii.gz", dcms))),
  ID = sub("[.]dcm([.]gz|)$", "", basename(dcms)))
df = df %>% 
  mutate(body = file.path("body", basename(nifti)),
         png = file.path("png", paste0(ID, ".png")))


ifold = as.numeric(Sys.getenv("SGE_TASK_ID"))
if (is.na(ifold)) {
  ifold = 1
}
df = df[ df$fold == ifold,]


sfnames = list.files(pattern = "slice_.*.nii.gz", 
                     path = "template_slices", 
                     full.names = TRUE)
if (padded) {
  sfnames = sfnames[ grepl("padded", sfnames)]
} else {
  sfnames = sfnames[ !grepl("padded", sfnames)]
}
slices = lapply(sfnames, antsImageRead)
make_oro_3d = function(x) {
  o = x
  o = array(o, dim = c(dim(o), 1))
  o = copyNIfTIHeader(img = x, arr = o, drop = FALSE)
  o
}

oro_slices = lapply(sfnames, function(x) {
  make_oro_3d(readnii(x, drop_dim = FALSE))
})
template = system.file("scct_unsmooth.nii.gz", 
                       package = "ichseg")
atemplate = antsImageRead(template)
mask = atemplate >= -1000
atemplate = maskImage(atemplate + 1024, mask)
antsSetOrigin(atemplate, c(0, 0, 0))
template_img = readnii(template)
mask = template_img >= -1000
template_img = mask_img(template_img + 1024, mask)
# all(df$ID %in% labs$ID)
# setdiff(df$ID, labs$ID)
run_df = df %>% 
  filter(file.exists(nifti) & file.exists(body))

iid = 1
for (iid in seq(nrow(run_df))) {
  print(iid)
  nifti_file = run_df$nifti[iid]
  body_outfile = run_df$body[iid]
  nii = readnii(nifti_file, drop_dim = FALSE)
  mask = readnii(body_outfile, drop_dim = FALSE)
  
  ##########################
  # masking out the non-body image (aka bed)
  ##########################
  body = mask_img(nii + 1024, mask)
  bimg = tempimg(body, drop_dim = FALSE)
  aimg = ANTsRCore::antsImageRead(bimg)
  arr = drop(aimg[,,1])
  amat = ANTsRCore::as.antsImage(arr, reference = aimg)
  # amat = amat + 1024
  antsSetOrigin(amat, c(0, 0))
  antsSetOrigin(aimg, c(0, 0, 0))
  if (padded) {
    mat = matrix(0, nrow = nrow(arr), ncol = ncol(arr))
    arr = abind::abind(mat, mat, as.array(amat), mat, mat, along = 3)
    amat = as.antsImage(arr, reference = aimg)
  }
  
  slice_indices = seq(10, 120, by = 5)
  run_slices = slices[slice_indices]
  run_oro_slices = oro_slices[slice_indices]
  reg = pbapply::pblapply(
    run_slices, function(r) {
      tfile = tempfile(fileext = ".nii.gz")
      reg = registration(
        amat, 
        template.file = r, 
        outfile = tfile,
        typeofTransform = "SyN", 
        interpolator = "Linear", retimg = FALSE,
        verbose = FALSE)
      reg$outfile = readnii(reg$outfile, 
                            drop_dim = FALSE)
      reg
    })
  
  oarr = lapply(reg, function(x) {
    make_oro_3d(x$outfile)
  })
  
  d = mapply(function(x, y) {
    mask = x > 0 | y > 0
    x = x[mask]
    y = y[mask]
    sqrt(mean(x - y)^2)
  }, oarr, run_oro_slices)
  
  check = run_oro_slices[[which.min(d)]]
  check_orig = oarr[[which.min(d)]]
  slice_overlay(check_orig, check, NA.y = TRUE)
  # slice_overlay(oarr[[1]]+1024, oro_slices[[1]]+1024, NA.y = TRUE)
}

