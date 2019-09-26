library(readr)
library(tidyr)
library(dplyr)
library(fs)
setwd(here::here())

n_folds = 200
x = list.files(path = "stage_1_test_images", 
               pattern = ".dcm", full.names = TRUE)
ids = sub("[.]dcm$", "", basename(x))
x = tibble::tibble(dcm = x,
                   ID = ids)
x$epidural = x$intraparenchymal = x$any = x$intraventricular = NA
x$subarachnoid = x$subdural =  NA
out = x %>% 
  select(-dcm)
write_rds(out, "stage_1_test.rds", compress = "xz")

x$nifti = path("nifti", basename(sub("[.]dcm([.]gz|)$", ".nii.gz", x$dcm)))

x = x %>% 
  mutate(body = file.path("body", basename(nifti)),
         png = file.path("png", paste0(ID, ".png")))

n_div = (ceiling(nrow(x) / n_folds))
x = x %>% 
  mutate(fold = floor(seq(nrow(x)) / n_div) + 1)
x$group = "test"

write_rds(x, "stage_1_test_data.rds", compress = "xz")

