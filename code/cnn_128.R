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
train = df %>% 
  arrange(scan_id, instance_number) %>% 
  filter(group == "train") %>% 
  filter(any > 0) 

tiffs = train %>% 
  pull(tiff)
ioutcome = c("epidural")
y = train %>% 
  select(one_of(ioutcome)) %>% 
  pull()
read_img = function(x) {
  img = EBImage::readImage(x)
  c(imageData(img))
}
mat = matrix(NA, nrow = nrow(train), ncol = 128*128)
all_data = pbapply::pbsapply()
