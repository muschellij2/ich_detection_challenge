library(readr)
library(tidyr)
library(dplyr)
library(fs)
setwd(here::here())

stage_number = 2

n_folds = 200

test_sample = readr::read_csv(paste0("stage_", stage_number,
                                     "_sample_submission.csv.gz"))
test_sample = test_sample %>% 
  mutate(sub = sub("ID_.*_(.*)", "\\1", ID),
         ID = sub("(ID_.*)_(.*)", "\\1", ID))
all_ids = unique(test_sample$ID)

x = list.files(path = paste0("stage_", stage_number, "_test_images"),
               pattern = ".dcm", full.names = TRUE)
ids = sub("[.]dcm$", "", basename(x))
x = tibble::tibble(dcm = x,
                   ID = ids)
x$epidural = x$intraparenchymal = x$any = x$intraventricular = NA
x$subarachnoid = x$subdural =  NA
out = x %>% 
  select(-dcm)
stopifnot(all(all_ids %in% x$ID))
write_rds(out, paste0("stage_", stage_number, "_test.rds"), compress = "xz")

x$nifti = path("nifti", basename(sub("[.]dcm([.]gz|)$", ".nii.gz", x$dcm)))

x = x %>% 
  mutate(body = file.path("body", basename(nifti)),
         png = file.path("png", paste0(ID, ".png")))

n_div = (ceiling(nrow(x) / n_folds))
x = x %>% 
  mutate(fold = floor(seq(nrow(x)) / n_div) + 1)
x$group = "test"

write_rds(x, paste0("stage_", stage_number, "_test_data.rds"), compress = "xz")

