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
test_outcomes = file.path("predictions", 
                          "test_outcomes.rds")
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
  
  test = df %>% 
    filter(group == "test") %>% 
    arrange(scan_id, instance_number) %>% 
    select(scan_id, image, one_of(outcomes), instance_number) %>% 
    mutate(new_group = "test")
  
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
  readr::write_rds(test, test_outcomes)
} else {
  train = readr::read_rds(train_outcomes)
  test = readr::read_rds(test_outcomes)
}
xlong = train %>% 
  ungroup() %>% 
  select(image, new_group, one_of(outcomes)) %>% 
  gather(outcome, value = value, -image, -new_group) %>% 
  mutate(
    new_file = file.path("cnn", 
                         outcome, 
                         new_group, 
                         ifelse(value == 1, "present", "absent"),
                         basename(image)))  
test_long = test %>% 
  ungroup() %>% 
  select(image, new_group, one_of(outcomes)) %>% 
  gather(outcome, value = value, -image, -new_group) %>% 
  mutate(
    new_file = file.path("cnn", 
                         outcome, 
                         new_group, 
                         basename(image)))  %>% 
  select(-value)

udn = unique(dirname(xlong$new_file))
tmp = sapply(udn, dir.create, recursive = TRUE, showWarnings = FALSE)

udn = unique(dirname(test_long$new_file))
tmp = sapply(udn, dir.create, recursive = TRUE, showWarnings = FALSE)

iscen = as.numeric(Sys.getenv("SGE_TASK_ID"))
if (is.na(iscen)) {
  iscen = 3
}
ioutcome = outcomes[iscen]

# for (ioutcome in outcomes) {
if (ioutcome != "any") {
  
  copy_long = function(long) {
    fe = file_exists(long$new_file)
    if (any(!fe)) {
      fe_original = file_exists(long$image)
      to_copy = long[!fe & fe_original, ]
      print(nrow(to_copy))
      for (iimage in seq(nrow(to_copy))) {
        print(iimage)
        file.copy(to_copy$image[iimage], to_copy$new_file[iimage])
      }
    }
  }
  
  long = xlong
  print(ioutcome)
  long = long %>% 
    filter(outcome == ioutcome)
  copy_long(long)
  
  long = test_long
  print(ioutcome)
  long = long %>% 
    filter(outcome == ioutcome)
  copy_long(long)
}
# }

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

