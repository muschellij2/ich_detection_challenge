library(dplyr)
library(fs)
library(readr)
library(tidyr)
setwd(here::here())
source("code/file_exists.R")
stage_number = 1
df = readr::read_rds(paste0("stage_", stage_number, "_data.rds"))

n_folds = 200

stage_number = 2
pre = ifelse(stage_number == 1, "", "stage2_")

outfiles = file.path("hdr", paste0(pre, "fold_", seq(n_folds), ".rds"))
outfiles = outfiles[file.exists(outfiles)]

wide_outfiles = file.path("wide_hdr", paste0(pre, "fold_", seq(n_folds), ".rds"))
wide_outfiles = wide_outfiles[file.exists(wide_outfiles)]
length(wide_outfiles)


wide = purrr::map_df(wide_outfiles, readr::read_rds)
wide = wide %>% 
  mutate_at(vars(BitsAllocated, BitsStored, Columns, HighBit,
                 PixelRepresentation, RescaleIntercept,
                 RescaleSlope, Rows, SamplesPerPixel), 
            parse_number_no_na) %>% 
  mutate_if(is.character, sub_bracket)
n_patients = length(unique(wide$PatientID))
print(paste0(n_patients, " patients in this data set"))
sub = wide %>% 
  group_by(PatientID, SeriesInstanceUID) %>% 
  tally()

wide = wide %>% 
  mutate(ID = sub("[.]dcm", "", basename(file)))

stopifnot(all(df$ID %in% wide$ID))

wide = wide %>% 
  mutate(ipp = gsub("\\\\", ",", ImagePositionPatient)) %>% 
  tidyr::separate(ipp, into = c("x", "y", "z"), remove = FALSE, sep = ",") %>% 
  mutate_at(vars(x, y, z), parse_number_no_na)

readr::write_rds(wide, path = paste0(pre, "wide_headers.rds"))

sub = wide %>% 
  select(ID, PatientID, SeriesInstanceUID,
         StudyInstanceUID, file)
readr::write_rds(sub, path = paste0(pre, "id_patient_map.rds"))
rm(wide)

# sub %>% 
#   group_by(PatientID, SeriesInstanceUID) %>% 
#   tally()
# sub %>% 
#   group_by(PatientID) %>% 
#   summarize(n_series = length(unique(SeriesInstanceUID))) %>% 
#   ungroup %>% 
#   count(n_series)


res = purrr::map_df(outfiles, readr::read_rds)
res = res %>% 
  mutate(ID = sub("[.]dcm", "", basename(file)))
stopifnot(all(df$ID %in% unique(res$ID)))

readr::write_rds(res, path = paste0(pre, "all_headers.rds"), compress = "xz")



