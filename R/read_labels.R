library(readr)
library(tidyr)
library(dplyr)
library(fs)
setwd(here::here())

source("code/file_exists.R")
stage_number = 2

n_folds = 200
on_cluster = function() {
  Sys.info()[["user"]] == "jmuschel"
}
check_gz = !on_cluster()

setwd(here::here())
x = readr::read_csv(paste0("stage_", stage_number, "_train.csv.gz"))
x = x %>% 
  mutate(sub = sub("ID_.*_(.*)", "\\1", ID),
         ID = sub("(ID_.*)_(.*)", "\\1", ID))

x = x %>% 
  distinct()
x = x %>% spread(sub, value = Label)

stopifnot(!anyNA(x))

# make sure all values are 0/1
uvals = x %>% 
  select(-ID) %>% 
  c() %>% 
  unlist %>% 
  unique()

stopifnot(all(uvals %in% c(0, 1)))

# Make sure any coincides with checking all columns
rs = x %>% 
  select(-ID, -any) %>% 
  rowSums
rs = rs > 0
stopifnot(all(rs == x$any))

write_rds(x, paste0("stage_", stage_number, "_train.rds"), compress = "xz")



x$dcm = unique(paste0(x$ID, ".dcm"))
if (check_gz) {
  dcm_gz = paste0(x$dcms, ".gz")
  fe = file.exists(dcm_gz)
  x$dcm[fe] = dcm_gz[fe]
}
x$dcm = file.path(paste0("stage_", stage_number, "_train_images"), x$dcm)
x$nifti = path("nifti", basename(sub("[.]dcm([.]gz|)$", ".nii.gz", x$dcm)))
fe = file_exists(x$dcm)

stopifnot(all(fe))
x = x %>% 
  mutate(body = file.path("body", basename(nifti)),
         png = file.path("png", paste0(ID, ".png")))

n_div = (ceiling(nrow(x) / n_folds))
x = x %>% 
  mutate(fold = floor(seq(nrow(x)) / n_div) + 1)
x$group = "train"

write_rds(x, paste0("stage_", stage_number, "_train_data.rds"), compress = "xz")

test_data = paste0("stage_", stage_number, "_test_data.rds")
if (file.exists(test_data)) {
  print(paste0("writing out all data for stage ", stage_number))
  test = read_rds(test_data)
  out = bind_rows(x, test)
  out_file = paste0("stage_", stage_number, "_data.rds")
  write_rds(out, out_file, compress = "xz")
}
