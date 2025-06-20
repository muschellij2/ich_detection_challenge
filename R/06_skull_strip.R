library(neurobase)
library(ichseg)
library(tibble)
library(dplyr)
library(fs)
library(freesurfer)
library(tidyr)
library(readr)
library(here)
library(lubridate)
library(purrr)
source(here::here("R/utils.R"))
rerun = FALSE

file_with_ss = here::here("data", "series_filesnames.rds")
if (!file.exists(file_with_ss) || rerun) {
  outfile = here::here("data", "series_data.rds")
  df = readr::read_rds(outfile)
  
  endings = c("", "_synth", "_original", "_ctbet", "_hdctbet", "_brainchop")
  all_dirs = lapply(endings, function(ending) {
    list(
      dir_ss = here::here("data", paste0("brain_extracted", ending)),
      dir_mask = here::here("data", paste0("brain_mask", ending)),
      dir_image = here::here("results", paste0("image_ss", ending))
    )
  })
  names(all_dirs) = c("v2", "synth", "original", "ctbet", "hdctbet", "brainchop")
  
  
  fs::dir_create(unlist(all_dirs))
  
  
  df = df %>%
    mutate(
      stub = basename(file_nifti),
      file_ss = here::here(all_dirs$v2$dir_ss, stub),
      file_mask = here::here(all_dirs$v2$dir_mask, stub),
      file_image_ss = here::here(all_dirs$v2$dir_image, paste0(nii.stub(stub), ".png")),
      
      file_ss_original = here::here(all_dirs$original$dir_ss, stub),
      file_mask_original = here::here(all_dirs$original$dir_mask, stub),
      file_image_ss_original = here::here(all_dirs$original$dir_image, paste0(nii.stub(stub), ".png")),
      
      file_ss_hdctbet = here::here(all_dirs$hdctbet$dir_ss, stub),
      file_mask_hdctbet = here::here(all_dirs$hdctbet$dir_mask, stub),
      file_image_ss_hdctbet = here::here(all_dirs$hdctbet$dir_image, paste0(nii.stub(stub), ".png")),
      
      file_ss_ctbet = here::here(all_dirs$ctbet$dir_ss, stub),
      file_mask_ctbet = here::here(all_dirs$ctbet$dir_mask, stub),
      file_image_ss_ctbet = here::here(all_dirs$ctbet$dir_image, paste0(nii.stub(stub), ".png")),
      
      file_ss_brainchop = here::here(all_dirs$brainchop$dir_ss, stub),
      file_mask_brainchop = here::here(all_dirs$brainchop$dir_mask, stub),
      file_image_ss_brainchop = here::here(all_dirs$brainchop$dir_image, paste0(nii.stub(stub), ".png")),
      
      file_ss_synth = here::here(all_dirs$synth$dir_ss, stub),
      file_mask_synth = here::here(all_dirs$synth$dir_mask, stub),
      file_image_ss_synth = here::here(all_dirs$synth$dir_image, paste0(nii.stub(stub), ".png"))
      
    ) %>%
    select(-stub)
  readr::write_rds(df, file_with_ss)
} else {
  df = readRDS(file_with_ss)
}

long = df %>% 
  mutate(id = nii.stub(file_nifti, bn = TRUE)) %>% 
  select(id, fold, starts_with("file_mask"), starts_with("file_ss")) %>% 
  tidyr::pivot_longer(
    cols = c(starts_with("file_mask"), starts_with("file_ss")),
    names_to = "type",
    values_to = "file"
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
  file_mask = idf$file_mask
  file_ss = idf$file_ss
  
  file_mask_original = idf$file_mask_original
  file_ss_original = idf$file_ss_original
  
  file_mask_synth = idf$file_mask_synth
  file_ss_synth = idf$file_ss_synth
  
  if (!all(file.exists(c(file_ss, file_mask)))) {
    ss.template.file =
      system.file("scct_unsmooth_SS_0.01.nii.gz",
                  package = "ichseg")
    ss.template.mask =
      system.file("scct_unsmooth_SS_0.01_Mask.nii.gz",
                  package = "ichseg")
    
    ss = try({
      CT_Skull_Strip_robust(
        img = file_nifti,
        retimg = FALSE,
        keepmask = TRUE,
        template.file = ss.template.file,
        template.mask = ss.template.mask,
        # remover = "double_remove_neck",
        outfile = file_ss,
        maskfile = file_mask)
    })
    if (inherits(ss, "try-error")) {
      # if it fails, try again with a copy of the qform
      img_run = copy_qform(file_nifti)
      ss = try({
        CT_Skull_Strip_robust(
          img = img_run,
          retimg = FALSE,
          keepmask = TRUE,
          template.file = ss.template.file,
          template.mask = ss.template.mask,
          # remover = "double_remove_neck",
          outfile = file_ss,
          maskfile = file_mask)      
      })
    }
  }
  
  if (!all(file.exists(c(file_ss_original, file_mask_original)))) {
    
    ss = try({
      # original
      CT_Skull_Strip(
        img = file_nifti,
        retimg = FALSE,
        keepmask = TRUE,
        # remover = "double_remove_neck",
        outfile = file_ss_original,
        maskfile = file_mask_original)
    })
    if (inherits(ss, "try-error")) {
      # if it fails, try again with a copy of the qform
      img_run = copy_qform(file_nifti)
      ss = try({
        CT_Skull_Strip(
          img = img_run,
          retimg = FALSE,
          keepmask = TRUE,
          # remover = "double_remove_neck",
          outfile = file_ss_original,
          maskfile = file_mask_original)    
      })
    }
  }
  
  
  if (!all(file.exists(c(file_ss_synth, file_mask_synth)))) {
    try({
      res = freesurfer::mri_synthstrip(
        file = file_nifti,
        retimg = FALSE,
        outfile = file_ss_synth,
        maskfile = file_mask_synth
      )
      if (res > 0 && !file.exists(file_ss_synth)) {
        img_run = copy_qform(file_nifti)
        res = freesurfer::mri_synthstrip(
          file = img_run,
          retimg = FALSE,
          outfile = file_ss_synth,
          maskfile = file_mask_synth
        )
      }
    })
  }
  
}

