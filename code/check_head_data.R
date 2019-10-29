library(readr)
library(dplyr)
setwd(here::here())

n_folds = 200
df = readr::read_rds("wide_headers.rds")

# add the group
df = df %>% 
  mutate(group = ifelse(grepl("train", file), "train", "test"),
         scan_id = paste0(PatientID, "-", SeriesInstanceUID))

# make sure we can just use series ID to 
# make unique ID
tt = df %>% 
  group_by(PatientID, SeriesInstanceUID) %>% 
  summarise(n = length(unique(StudyInstanceUID)))
stopifnot(all(tt$n == 1))

tt = df %>% 
  group_by(PatientID, StudyInstanceUID) %>% 
  summarise(n = length(unique(SeriesInstanceUID)))
stopifnot(all(tt$n == 1))

##################################
# Sort the data
##################################
df = df %>% 
  arrange(PatientID, StudyInstanceUID, SeriesInstanceUID, x, y, z) 

##################################
# create folds
##################################
id = df %>% 
  select(PatientID, StudyInstanceUID, SeriesInstanceUID, scan_id) %>% 
  arrange(PatientID, StudyInstanceUID, SeriesInstanceUID) %>% 
  distinct() %>% 
  mutate(index = seq(n()),
         ss_file = file.path("ss", paste0(scan_id, ".nii.gz")),
         ss_robust_file  = file.path("ss_robust", paste0(scan_id, ".nii.gz")),
         robust_maskfile  = file.path("ss_robust", paste0(scan_id, "_Mask.nii.gz")),
         maskfile = file.path("mask", paste0(scan_id, ".nii.gz")),
         outfile = file.path("nifti", paste0(scan_id, ".nii.gz")),
         alt_outfile = file.path("eq_nifti", paste0(scan_id, ".nii.gz")),
         predfile = file.path("pred", paste0(scan_id, ".nii.gz")),
         reg_file = file.path("reg", paste0(scan_id, ".nii.gz")),
         alt_ss_file = sub("ss/", "eq_ss/", ss_file),
         alt_ss_maskfile = sub("[.]nii", "_Mask.nii", alt_ss_file),
         reg_mat = file.path("reg", paste0(scan_id, ".mat")),
         pngfile = file.path("png", sub("[.]nii.gz", ".png", basename(outfile))),
         robust_pngfile = sub("png/", "png_robust/", pngfile)
  )
id$robust_reg_file = sub("reg/", "reg_robust/", id$reg_file)
id$robust_reg_mat = sub("reg/", "reg_robust/", id$reg_mat)

n_div = (ceiling(nrow(id) / n_folds))
id = id %>% 
  mutate(fold = floor(seq(nrow(id)) / n_div) + 1)
# n_series = 21744
df = left_join(df, id)


##################################
# check how many slices at same spot
##################################
df = df %>%   
  arrange(PatientID, StudyInstanceUID, SeriesInstanceUID, z) %>% 
  group_by(PatientID, StudyInstanceUID, SeriesInstanceUID) %>% 
  mutate(instance_number = seq(n())) %>% 
  ungroup()

df = df %>%   
  group_by(PatientID, SeriesInstanceUID, x, y, z) %>% 
  mutate(repeat_index = seq(n())) %>% 
  ungroup()

##################################
# Sort the data
##################################
df = df %>% 
  arrange(PatientID, StudyInstanceUID, SeriesInstanceUID, x, y, z) 


readr::write_rds(df, "wide_headers_with_folds.rds")

hdr = readr::read_rds("wide_headers_with_folds.rds")

outcomes = readr::read_rds("stage_1_data.rds")
outcomes = outcomes %>%
  select(ID,
         any,
         epidural,
         intraparenchymal,
         intraventricular,
         subarachnoid,
         subdural)

stopifnot(
  length(setdiff(hdr$ID, outcomes$ID)) == 0,
  length(setdiff(outcomes$ID, hdr$ID)) == 0
)

outcomes = full_join(outcomes, hdr)

outcomes = outcomes  %>% 
  arrange(PatientID, StudyInstanceUID, SeriesInstanceUID, x, y, z) 

readr::write_rds(outcomes, "wide_headers_with_folds_outcomes.rds")

# dup_data =  df %>%   
#   arrange(PatientID, StudyInstanceUID, SeriesInstanceUID, 
#           x, y, z, instance_number) %>% 
#   group_by(PatientID, StudyInstanceUID, SeriesInstanceUID, x, y, z) %>% 
#   filter(n() > 1)
outcomes = c("any", "epidural", "intraparenchymal", "intraventricular", 
             "subarachnoid", "subdural")
##################################
# finding when you have duplicate slices
##################################
dup_data =  df %>%   
  group_by(SeriesInstanceUID, PatientID) %>% 
  filter(!all(n_index == 1))

dup_data = dup_data %>% 
  select(ID, file, group, SeriesInstanceUID, PatientID, n_index, 
         fold, x, y, z, index)
  
  
  ##################################
# Need to check on these IDs for each fold - must subset
##################################
check_folds = dup_data %>% 
  ungroup() %>% 
  select(index, fold) %>% 
  distinct()


# check if any duplications are all train or test!
# Make sure not crossing the train/test designation
check = df %>%   
  group_by(SeriesInstanceUID, PatientID) %>% 
  summarise(dup = !all(n_index == 1),
            pct_train = mean(group == "train")) %>% 
  ungroup()
# make sure all train or all test (no half brain)
stopifnot(all(check$pct_train %in% c(0, 1)))

##################################
# see duplication across sets
##################################
check %>% 
  count(dup, pct_train)


s = df %>% 
  group_by(scan_id) %>% 
  summarise(n_train = mean(group == "train"))

