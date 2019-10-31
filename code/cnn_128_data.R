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

if (!file.exists(train_outcomes)) {
  df = readr::read_rds("wide_headers_with_folds_outcomes.rds")
  df = df %>% 
    mutate(tiff = file.path(
      "tiff_128", 
      sub(".dcm", ".tiff", basename(file))))
  
  train = df %>% 
    arrange(scan_id, instance_number) %>% 
    filter(group == "train") %>% 
    filter(any > 0) 
  rm(df)
  train = train %>% 
    select(scan_id, tiff, one_of(outcomes))
  train = train %>% 
    group_by(scan_id) %>% 
    mutate(prob = runif(1),
           new_group = ifelse(prob >= keep_pct, "train", "validation"),
           new_file = file.path(new_group, basename(tiff)))
  
  readr::write_rds(train, train_outcomes)
} else {
  train = readr::read_rds(train_outcomes)
}

fe = file_exists(train$new_file)
if (any(!fe)) {
  to_copy = train[!fe, ]
  file.copy(to_copy$tiff, to_copy$new_file)
}

outfile = file.path("predictions", "cnn_128_data.rds")

if (!file.exists(outfile)) {
  
  tiffs = train %>% 
    pull(tiff)
  read_img = function(x) {
    img = EBImage::readImage(x)
    c(imageData(img))
  }
  mat = matrix(NA_real_, nrow = nrow(train), ncol = 128*128)
  for (irow in 1:nrow(train)) {
    print(irow)
    img = read_img(tiffs[irow])
    mat[irow, ] = img
  }
  
  readr::write_rds(mat, 
                   path = outfile)
  
} else {
  mat = readr::read_rds(outfile)
}

