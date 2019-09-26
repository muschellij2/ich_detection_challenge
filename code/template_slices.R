library(neurobase)
library(ANTsRCore)
library(extrantsr)
library(ichseg)
setwd(here::here())

template = system.file("scct_unsmooth.nii.gz", 
                       package = "ichseg")
atemplate = antsImageRead(template)
antsSetOrigin(atemplate, c(0, 0, 0))
ind = which(atemplate > 0, arr.ind = TRUE)
ind = unique(ind[,3])
mask = atemplate >= -1000
atemplate = maskImage(atemplate + 1024, mask)

slices = lapply(ind, function(x) {
  arr = atemplate[,,x]
  arr = drop(arr)
  xx = as.antsImage(arr, reference = atemplate)
  fname = here::here("template_slices",
                     sprintf("slice_%03.0f.nii.gz", x))
  antsImageWrite(xx, fname)
  xx
})

mat = matrix(0, nrow = nrow(atemplate), ncol = ncol(atemplate))
padded_slices = lapply(ind, function(x) {
  arr = atemplate[,,x]
  arr = abind::abind(mat, mat, arr, mat, mat)
  xx = as.antsImage(arr, reference = atemplate)
  fname = here::here("template_slices", 
                     sprintf("padded_slice_%03.0f.nii.gz", x))
  antsImageWrite(xx, fname)
  xx
})
