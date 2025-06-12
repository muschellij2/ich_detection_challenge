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
  file_nifti_uncorrected = series$file_nifti_uncorrected[[iid]]
  idf = series$data[[iid]]
  
  if (!file.exists(file_nifti)) {
    idf = idf %>% filter(id != "ID_6431af929")
    res = create_nifti(idf)
    if (is.null(res)) {
      print(iid)
      print(file_nifti)
      idf = idf %>% 
        arrange(file) %>% 
        group_by(
          ImageOrientationPatient, 
          ImagePositionPatient,
          SeriesInstanceUID
        ) %>% 
        summarise(
          file = file[1],
          .groups = "drop"
        ) 
      res = create_nifti(idf)
    }
    if (!is.null(res)) {
      writenii(res, file_nifti)
    }
  }
  
  if (!file.exists(file_nifti_uncorrected)) {
    idf = idf %>% filter(id != "ID_6431af929")
    res = create_nifti(idf, uncorrected = TRUE)
    if (is.null(res)) {
      print(iid)
      print(file_nifti)
      idf = idf %>% 
        arrange(file) %>% 
        group_by(
          ImageOrientationPatient, 
          ImagePositionPatient,
          SeriesInstanceUID
        ) %>% 
        summarise(
          file = file[1],
          .groups = "drop"
        ) 
      res = create_nifti(idf, uncorrected = TRUE)
    }
    if (!is.null(res)) {
      writenii(res, file_nifti_uncorrected)
    }
  }
  
}



