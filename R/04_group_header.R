library(dplyr)
library(fs)
library(readr)
library(tidyr)
source(here::here("R/utils.R"))

filename = here::here("data", "dicom_filenames.rds")
df = readr::read_rds(filename)

headers = df$hdr

outfile = here::here("data", "dicom_headers.rds")
if (!file.exists(outfile)) {
  wide = purrr::map_df(headers, readr::read_rds, .progress = TRUE)
  wide$file = sub("/legacy/dexter/disk2/smart/stroke_ct/ident",
                  "/dcs05/ciprian/smart", wide$file)
  sub_df = df %>% 
    select(file, stage_number, group, id, fold)
  wide = wide %>% left_join(sub_df)
  readr::write_rds(wide, outfile)
} else {
  wide = readr::read_rds(outfile)
}
study = wide %>% 
  nest(data = everything(), .by = StudyInstanceUID)

series = wide %>% 
  nest(data = everything(), .by = SeriesInstanceUID)


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



