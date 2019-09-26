library(readr)
library(tidyr)
library(dplyr)
library(fs)
setwd(here::here())

n_folds = 200
on_cluster = function() {
  Sys.info()[["user"]] == "jmuschel"
}
check_gz = !on_cluster()

setwd(here::here())
x = read_csv("stage_1_train.csv.gz")
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

write_rds(x, "stage_1_train.rds", compress = "xz")



x$dcm = unique(paste0(x$ID, ".dcm"))
if (check_gz) {
  dcm_gz = paste0(x$dcms, ".gz")
  fe = file.exists(dcm_gz)
  x$dcm[fe] = dcm_gz[fe]
}
x$dcm = file.path("stage_1_train_images", x$dcm)
x$nifti = path("nifti", basename(sub("[.]dcm([.]gz|)$", ".nii.gz", x$dcm)))

x = x %>% 
  mutate(body = file.path("body", basename(nifti)),
         png = file.path("png", paste0(ID, ".png")))

n_div = (ceiling(nrow(x) / n_folds))
x = x %>% 
  mutate(fold = floor(seq(nrow(x)) / n_div) + 1)
x$group = "train"

write_rds(x, "stage_1_train_data.rds", compress = "xz")

if (file.exists("stage_1_test_data.rds")) {
  test = read_rds("stage_1_test_data.rds")
  out = bind_rows(x, test)
  write_rds(out, "stage_1_data.rds", compress = "xz")
}
