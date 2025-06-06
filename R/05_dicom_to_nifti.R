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
outfile = here::here("data", "dicom_headers.rds")
df = readr::read_rds(outfile)

n_ids = df %>% 
  group_by(StudyInstanceUID) %>% 
  summarise(n = n_distinct(PatientID))

stopifnot(all(n_ids$n == 1))

n_study = df %>% 
  group_by(PatientID) %>% 
  summarise(n = n_distinct(StudyInstanceUID))

n_study_per_series = df %>% 
  group_by(SeriesInstanceUID) %>% 
  summarise(n = n_distinct(StudyInstanceUID))

stopifnot(all(n_study_per_series$n == 1))


df = df %>% 
  mutate(
    id_patient = remove_brackets(PatientID),
    id_patient = gsub_under(id_patient),
    id_series = remove_brackets(SeriesInstanceUID),
    id_series = gsub_under(id_series),
    
    file_nifti = file.path(
      here::here("data", "nifti"),
      paste0(
        id_patient, "_",
        id_series, 
        ".nii.gz"
      )
    )
  )

n_folds = 200
id_df = df %>% 
  distinct(id_patient) %>% 
  mutate(
    fold = seq(dplyr::n()),
    fold = floor(fold / ceiling(dplyr::n()/n_folds) + 1)
) 
df = df %>% 
  left_join(id_df, by = "id_patient") 

xdf = df

df = xdf
ifold = get_fold(default = unique(df$fold))
df = df %>%
  filter(fold %in% ifold)

series = df %>% 
  nest(data = everything(), .by = file_nifti)

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


