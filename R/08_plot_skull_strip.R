library(neurobase)
library(tibble)
library(dplyr)
library(fs)
source(here::here("R/utils.R"))
file_with_ss = here::here("data", "series_filesnames.rds")
df = readRDS(file_with_ss)

endings = c("", "_synth", "_original", "_ctbet", "_hdctbet")
all_dirs = lapply(endings, function(ending) {
  list(
    dir_ss = here::here("data", paste0("brain_extracted", ending)),
    dir_mask = here::here("data", paste0("brain_mask", ending)),
    dir_image = here::here("results", paste0("image_ss", ending))
  )
})
names(all_dirs) = c("v2", "synth", "original", "ctbet", "hdctbet")



plot_seg = function(
    file_nifti,
    file_mask,
    file_image_ss) {
  if (!all(file.exists(c(file_image_ss))) &
      file.exists(file_mask)) {
    img = readnii(file_nifti)
    img = window_img(img, c(0, 100))
    mask = readnii(file_mask)
    mask = mask > 0
    dir.create(dirname(file_image_ss), recursive = TRUE, showWarnings = FALSE)
    # write some text to show what image
    text = nii.stub(file_nifti, bn = TRUE)
    text = sub("_CT_", "_", text)
    text = gsub("_", "\n", text)
    png(file_image_ss, res = 300, width = 2000, height = 1000)
    ortho2(
      img,
      mask,
      NA.y = TRUE,
      col.y = scales::alpha("red", 0.5),
      text = text
    )
    dev.off()
  }
  
}

iid = get_fold()

# for (iid in seq(nrow(df))) {
print(iid)
idf = df[iid,]

file_nifti = idf$file_nifti

file_mask = idf$file_mask
file_image_ss = idf$file_image_ss

plot_seg(file_nifti,
         file_mask,
         file_image_ss)


file_image_ss = idf$file_image_ss_original
file_mask = idf$file_mask_original

plot_seg(file_nifti,
         file_mask,
         file_image_ss)


file_image_ss = idf$file_image_ss_synth
file_mask = idf$file_mask_synth

plot_seg(file_nifti,
         file_mask,
         file_image_ss)

file_image_ss = idf$file_image_ss_hdctbet
file_mask = idf$file_mask_hdctbet

plot_seg(file_nifti,
         file_mask,
         file_image_ss)


file_image_ss = idf$file_image_ss_ctbet
file_mask = idf$file_mask_ctbet

plot_seg(file_nifti,
         file_mask,
         file_image_ss)





# }
