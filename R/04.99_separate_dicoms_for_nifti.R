library(neurobase)
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

# ifold = get_fold(default = unique(series$fold))
# print(head(ifold))
# series = series %>%
#   filter(fold %in% ifold)
# print(nrow(series))
# series = series %>% 
#   mutate(
#     id = paste0(
#       id_patient, "_",
#       id_series),
#     dir_dicom = 
#   )


iid = 1
for (iid in seq(nrow(series))) {
  print(iid)
  id = nii.stub(series$file_nifti[[iid]], bn = TRUE)
  id_dir = here::here("data", "dicom_separated", id)
  dir.create(id_dir, recursive = TRUE, showWarnings = FALSE)
  file_nifti_uncorrected = series$file_nifti_uncorrected[[iid]]
  idf = series$data[[iid]]
  idf$outfile = file.path(id_dir, basename(idf$file))
  
  file.copy(idf$file, idf$outfile, overwrite = FALSE)
  
}



