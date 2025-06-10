library(neurobase)
library(ichseg)
library(tibble)
library(dplyr)
library(fs)
library(oro.nifti)
library(png)
library(grid)
library(gridExtra)
source(here::here("R/utils.R"))
outfile = here::here("data", "series_data.rds")
df = readr::read_rds(outfile)
dir_image = here::here("results", "image")
fs::dir_create(dir_image)


df = df %>%
  mutate(
    stub = nii.stub(file_nifti, bn = TRUE),
    file_image = file.path(dir_image, paste0(stub, ".png"))
  )


ifold = get_fold(default = unique(df$fold))
print(head(ifold))
df = df %>%
  filter(fold %in% ifold)
print(nrow(df))

iid = 1

for (iid in seq(nrow(df))) {
  print(iid)
  idf = df[iid,]
  
  file_nifti = idf$file_nifti
  file_image = idf$file_image
  
  if (!file.exists(file_image)) {
    img = readnii(file_nifti)
    img = window_img(img, c(0, 100))
    
    # write some text to show what image
    text = nii.stub(file_nifti, bn = TRUE)
    text = sub("_", "\n", text)
    text = gsub("ID-", "", text)
    
    png(file_image, res = 300, width = 2000, height = 1000)
    ortho2(
      img,
      text = text
    )
    dev.off()
    
  }
}
