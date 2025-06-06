library(dcm2niir)
library(neurobase)
library(ichseg)
library(dplyr)
library(fs)
library(tidyr)
library(readr)
library(here)
library(lubridate)
library(purrr)
source(here::here("R/utils.R"))
outfile = here::here("data", "series_data.rds")
series = readr::read_rds(outfile)

ifold = get_fold(default = unique(series$fold))
print(head(ifold))
series = series %>%
  filter(fold %in% ifold)
print(nrow(series))
iid = 1
for (iid in seq(nrow(series))) {
  print(iid)
  file_nifti = series$file_nifti[[iid]]
  idf = series$data[[iid]]
  if (!file.exists(file_nifti)) {
    out = try({copy_dcm_files(idf)})
    if (inherits(out, "try-error")) {
      next
    }
    tdir = out$outdir
    file_df = out$file_df
    res = try({
      ct_dcm2nii(tdir,
                 verbose = FALSE,
                 dcm2niicmd = "dcm2niix_feb2024",
                 ignore_roi_if_multiple = TRUE,
                 fail_on_error = TRUE)
    })
    unlink(tdir, recursive = TRUE)
    if (length(dim(res)) != 3 || inherits(res, "try-error")) {
      next
    }
    writenii(res, file_nifti)
  }
}


