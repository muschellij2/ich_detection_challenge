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
        maskfile = file.path("mask", paste0(scan_id, ".nii.gz")),
        outfile = file.path("nifti", paste0(scan_id, ".nii.gz")))

n_div = (ceiling(nrow(id) / n_folds))
id = id %>% 
  mutate(fold = floor(seq(nrow(id)) / n_div) + 1)
# n_series = 21744
df = left_join(df, id)


##################################
# check how many slices at same spot
##################################
df = df %>%   
  group_by(SeriesInstanceUID, PatientID, x, y, z) %>% 
  mutate(n_index = seq(n())) %>% 
  ungroup()

readr::write_rds(df, "wide_headers_with_folds.rds")


##################################
# finding when you have duplicate slices
##################################
dup_data =  df %>%   
  group_by(SeriesInstanceUID, PatientID) %>% 
  filter(!all(n_index == 1))

dup_data = dup_data %>% 
  select(ID, file, group, SeriesInstanceUID, PatientID, n_index, 
         fold, x, y, z, index) %>% 


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

