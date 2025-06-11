library(dplyr)
library(fs)
library(readr)
library(tidyr)
library(neurobase)
library(tidyr)
source(here::here("R/utils.R"))

filename = here::here("data", "dicom_filenames.rds")
df = readr::read_rds(filename)

headers = df$hdr

outfile = here::here("data", "dicom_headers.rds")
if (!file.exists(outfile)) {
  print(length(headers))
  wide = purrr::map_df(headers, readr::read_rds, .progress = TRUE)
  wide$id = sub("[.]dcm", "", basename(wide$file))
  wide$file = NULL
  sub_df = df %>% 
    select(file, group, id, fold)
  wide = wide %>% left_join(sub_df) %>% 
    select(file, id, group, everything())
  readr::write_rds(wide, outfile)
} else {
  wide = readr::read_rds(outfile)
}
study = wide %>% 
  nest(data = everything(), .by = StudyInstanceUID)

series = wide %>% 
  nest(data = everything(), .by = SeriesInstanceUID)


df = wide

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
    ),
    file_nifti_uncorrected = file.path(
      here::here("data", "nifti_uncorrected"),
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
  # fold is now by image, not by file
  select(-fold) %>% 
  left_join(id_df, by = "id_patient") 

outfile = here::here("data", "series_data.rds")
check = df %>% 
  distinct(id_patient, id_series, file_nifti, file_nifti_uncorrected, group) 
stopifnot(anyDuplicated(check$file_nifti) == 0)
stopifnot(anyDuplicated(check$file_nifti_uncorrected) == 0)


series = df %>% 
  nest(data = everything(), .by = c(id_patient, id_series, file_nifti, file_nifti_uncorrected, group, fold))
readr::write_rds(series, outfile, compress = "xz")


# 
#   
# wide = wide %>% 
#   mutate_at(vars(BitsAllocated, BitsStored, Columns, HighBit,
#                  PixelRepresentation, RescaleIntercept,
#                  RescaleSlope, Rows, SamplesPerPixel), 
#             parse_number_no_na) %>% 
#   mutate_if(is.character, sub_bracket)
# n_patients = length(unique(wide$PatientID))
# print(paste0(n_patients, " patients in this data set"))
# sub = wide %>% 
#   group_by(PatientID, SeriesInstanceUID) %>% 
#   tally()
# 
# wide = wide %>% 
#   mutate(ID = sub("[.]dcm", "", basename(file)))
# 
# stopifnot(all(df$ID %in% wide$ID))
# 
# wide = wide %>% 
#   mutate(ipp = gsub("\\\\", ",", ImagePositionPatient)) %>% 
#   tidyr::separate(ipp, into = c("x", "y", "z"), remove = FALSE, sep = ",") %>% 
#   mutate_at(vars(x, y, z), parse_number_no_na)
# 
# readr::write_rds(wide, path = paste0(pre, "wide_headers.rds"))
# 
# sub = wide %>% 
#   select(ID, PatientID, SeriesInstanceUID,
#          StudyInstanceUID, file)
# readr::write_rds(sub, path = paste0(pre, "id_patient_map.rds"))
# rm(wide)
# 
# # sub %>% 
# #   group_by(PatientID, SeriesInstanceUID) %>% 
# #   tally()
# # sub %>% 
# #   group_by(PatientID) %>% 
# #   summarize(n_series = length(unique(SeriesInstanceUID))) %>% 
# #   ungroup %>% 
# #   count(n_series)
# 
# 
# res = purrr::map_df(outfiles, readr::read_rds)
# res = res %>% 
#   mutate(ID = sub("[.]dcm", "", basename(file)))
# stopifnot(all(df$ID %in% unique(res$ID)))
# 
# readr::write_rds(res, path = paste0(pre, "all_headers.rds"), compress = "xz")



