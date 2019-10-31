rm(list = ls())
library(EBImage)
library(dplyr)
library(tidyr)
setwd(here::here())
source("code/file_exists.R")
df = readr::read_rds("wide_headers_with_folds_outcomes.rds")
df = df %>% 
  mutate(tiff = file.path(
    "tiff_128", 
    sub(".dcm", ".tiff", basename(file))))

outcomes = c("any", "epidural", "intraparenchymal", "intraventricular", 
             "subarachnoid", "subdural")

train = df %>% 
  arrange(scan_id, instance_number) %>% 
  filter(group == "train") %>% 
  filter(any > 0) 
rm(df)

outfile = file.path("predictions", "cnn_128_data.rds")

if (!file.exists(outfile)) {
  
  tiffs = train %>% 
    pull(tiff)
  ioutcome = c("epidural")
  y = train %>% 
    select(one_of(ioutcome)) %>% 
    pull()
  train = train %>% 
    select(scan_id, tiff, one_of(outcomes))
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
  
}
