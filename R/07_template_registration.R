library(neurobase)
library(tibble)
library(dplyr)
library(fs)
library(extrantsr)
source(here::here("R/utils.R"))
file_with_ss = here::here("data", "series_filesnames.rds")
df = readRDS(file_with_ss)


dirs_to_create = c("template_registered", "template_transforms")
fs::dir_create(here::here("data", dirs_to_create))


df = df %>%
  mutate(
    file_nifti_registered = sub("/nifti/", "/template_registered/", file_nifti),
    file_nifti_registered_brain = sub("[.]nii", "_brain.nii", file_nifti_registered),
    file_nifti_registered_brainmask = sub("[.]nii", "_brainmask.nii", file_nifti_registered),
    file_registered_transforms = here::here("data", "template_transforms",
                                            nii.stub(file_nifti, bn = TRUE))
  )

df %>%
  select(contains("registered")) %>%
  unlist() %>%
  dirname() %>%
  unique() %>%
  fs::dir_create()

iid = get_fold()

for (iid in seq(nrow(df))) {
  print(iid)
  idf = df[iid,]
  
  file_nifti_brain = idf$file_ss
  file_nifti = idf$file_nifti
  file_nifti_brainmask = idf$file_mask
  
  file_nifti_registered = idf$file_nifti_registered
  file_nifti_registered_brain = idf$file_nifti_registered_brain
  file_nifti_registered_brainmask = idf$file_nifti_registered_brainmask
  file_registered_transforms = idf$file_registered_transforms
  
  # for (irow in seq(nrow(iid_df))) {
  
  if (
    !all(
      file.exists(
        c(file_nifti_registered,
          file_nifti_registered_brain, file_nifti_registered_brainmask
        )
      )
    )
  ) {
    # ss.template.file =
    #   system.file("scct_unsmooth_SS_0.01.nii.gz",
    #               package = "ichseg")
    ss.template.file = here::here("data", "template", "template_1mm.nii.gz")
    # ss.template.file = here::here("data", "template", "template_0.5mm.nii.gz")
    
    reg = registration(
      filename = file_nifti_brain,
      template.file = ss.template.file,
      typeofTransform = "Rigid",
      skull_strip = FALSE,
      correct = FALSE,
      retimg = TRUE,
      outfile = file_nifti_registered_brain,
      movingMask = file_nifti_brainmask,
      other.files = c(
        file_nifti,
        file_nifti_brainmask
      ),
      other.outfiles = c(
        file_nifti_registered,
        file_nifti_registered_brainmask
      ),
      other_interpolator = "NearestNeighbor",
      interpolator = "NearestNeighbor",
      remove.warp = FALSE,
      outprefix = file_registered_transforms
    )
    
  }
}
