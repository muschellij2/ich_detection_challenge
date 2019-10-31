rm(list = ls())
library(EBImage)
library(dplyr)
library(tidyr)
setwd(here::here())
source("code/file_exists.R")

set.seed(20191031)
outcomes = c("any", "epidural", "intraparenchymal", "intraventricular", 
             "subarachnoid", "subdural")


train_outcomes = file.path("predictions", 
                           "training_outcomes.rds")
keep_pct = 0.8
dir.create("cnn")
out_dirs = file.path("cnn", outcomes)
sapply(out_dirs, dir.create, showWarnings = FALSE)

out_type = "png"
if (!file.exists(train_outcomes)) {
  df = readr::read_rds("wide_headers_with_folds_outcomes.rds")
  df = df %>% 
    mutate(image = 
             file.path(paste0(out_type, "_128"),
                       sub(".dcm", 
                           paste0(".", out_type), 
                           basename(file))))
  
  train = df %>% 
    arrange(scan_id, instance_number) %>% 
    filter(group == "train") %>% 
    filter(any > 0) 
  rm(df)
  train = train %>% 
    select(scan_id, image, one_of(outcomes))
  train = train %>% 
    group_by(scan_id) %>% 
    mutate(prob = runif(1),
           new_group = ifelse(prob >= keep_pct, "train", "validation"),
           new_file = file.path(new_group, basename(image)))
  
  readr::write_rds(train, train_outcomes)
} else {
  train = readr::read_rds(train_outcomes)
}
long = train %>% 
  ungroup() %>% 
  select(image, new_group, one_of(outcomes)) %>% 
  select(-any) %>% 
  gather(outcome, value = value, -image, -new_group) %>% 
  mutate(
    new_file = file.path("cnn", 
                         outcome, 
                         new_group, 
                         ifelse(value == 1, "present", "absent"),
                         basename(image)))  

udn = unique(dirname(long$new_file))
tmp = sapply(udn, dir.create, recursive = TRUE, showWarnings = FALSE)

fe = file_exists(long$new_file)
fe_original = file_exists(long$image)
if (any(!fe)) {
  to_copy = long[!fe & fe_original, ]
  for (iimage in seq(nrow(to_copy))) {
    file.copy(to_copy$image[iimage], to_copy$new_file[iimage])
  }
}

outfile = file.path("predictions", "cnn_128_data.rds")

if (!file.exists(outfile)) {
  
  images = train %>% 
    pull(image)
  read_img = function(x) {
    img = EBImage::readImage(x)
    c(imageData(img))
  }
  mat = matrix(NA_real_, nrow = nrow(train), ncol = 128*128)
  for (irow in 1:nrow(train)) {
    print(irow)
    img = read_img(images[irow])
    mat[irow, ] = img
  }
  
  readr::write_rds(mat, 
                   path = outfile, 
                   compress = "xz")
  
} else {
  mat = readr::read_rds(outfile)
}

