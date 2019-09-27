rm(list = ls())
library(dcm2niir)
library(neurobase)
library(divest)
library(ichseg)
library(dplyr)
library(fs)
library(dcmtk)
setwd(here::here())

sub_bracket = function(x) {
  x = sub("^\\[", "", x)
  x = sub("\\]$", "", x)
  x = trimws(x)
}

tmp = sapply(c("ss", "mask", "nifti"), dir.create, 
             showWarnings = FALSE)

n_folds = 200

df = readr::read_rds("wide_headers_with_folds.rds")

all_df = df

df = all_df

ifold = as.numeric(Sys.getenv("SGE_TASK_ID"))
if (is.na(ifold)) {
  ifold = 74
}

df = df[ df$fold == ifold,]


iid = 1

uids = unique(df$index)
# iid =102
#iid = 219
for (iid in uids) {
  print(iid)
  run_df = df[ df$index == iid, ]
  
  
  
  ss_file = unique(run_df$ss_file)
  maskfile = unique(run_df$maskfile)
  outfile = unique(run_df$outfile)
  stopifnot(length(ss_file) == 1,
            length(maskfile) == 1,
            length(outfile) == 1)
  pngfile = file.path("png", 
                      sub("[.]nii.gz", ".png", basename(outfile)))
  
  
  
  run_df = run_df %>%   
    group_by(x, y, z, PatientID, SeriesInstanceUID) %>% 
    mutate(n_index = seq(n())) %>% 
    ungroup()
  if (!all(run_df$n_index == 1)) {
    warning("Duplicated data!")
  }
  run_df = run_df %>% 
    filter(n_index == 1)
  # make sure for ordering
  
  if (!all(file.exists(c(ss_file, maskfile, outfile)))) {
    
    run_df = run_df %>% 
      select(PatientID, StudyInstanceUID, SeriesInstanceUID, 
             x, y, z, everything()) %>% 
      arrange(PatientID, StudyInstanceUID, SeriesInstanceUID, z) %>% 
      ungroup() %>% 
      mutate(slice_order = seq(n()))
    stopifnot(all(diff(run_df$z) >= 0))
    
    tdir = tempfile()
    dir.create(tdir)
    file.copy(run_df$file, tdir)
    x = divest::scanDicom(path = tdir)
    stack_data = FALSE
    if (nrow(x) > 1) {
      # iid = 1968
      # ifold = 19
      # ID_17512399-ID_ca9c5d9ce1 - example
      # "ID_08f09333-ID_4fd74dabb2" is even worse
      x = divest::scanDicom(path = tdir, forceStack = TRUE)
      stack_data = TRUE
    }
    stopifnot(nrow(x) == 1)
    paths = attributes(x)$paths[[1]]
    
    stopifnot(all(basename(paths) == basename(run_df$file)))
    
    ind = seq_along(paths)
    add_instance = function(file, index) {
      dcmtk::dcmodify(
        file = file,
        frontopts = paste0('-i "(0020,0013)=', index, '"')
      )
      hdr = read_dicom_header(file)
      new_inst = as.numeric(sub_bracket(hdr$value[hdr$name == "InstanceNumber"]))
      stopifnot(new_inst == index)
      print(new_inst)
      bakfile = paste0(file, ".bak")
      if (file.exists(bakfile)) {
        file.remove(bakfile)
      }
    }
    res = mapply(add_instance, paths, ind)
    
    d_res = dcm2nii(basedir = tdir,
                    opts = paste0(
                      "-9 ",
                      " -v y ",
                      if (stack_data) "-m y",
                      " -z i -f %p_%t_%s"))
    nii = d_res$nii_after
    # need to remove these
    nii = nii[ !grepl("(Tilt|Eq)", nii)]
    stopifnot(length(nii) == 1)
    img = readnii(nii[1])
    # 3786 fails at 512x512
    # "nifti/ID_2b9e0826-ID_9224996075.nii.gz"
    # 8011 as well - not square too
    # 8011 is flipped
    # Make sure same dimensions!
    dimg = dim(img)[1:2]
    dim_512 = all(dimg == 512)
    if (!dim_512) {
      dims = lapply(paths, function(x) {
        dim(oro.dicom::readDICOMFile(x)$img)[1:2]
      })
      all_equal_dims = all(sapply(dims, function(x) {
        all(x == dims[[1]])
      }))
      # %in% not == for 8011 as LR flipping
      stopifnot(all(dimg %in% dims[[1]]))
      # udim = unique(dim(img)[1:2])
      # square = length(udim) == 1
      # stopifnot(square)
      # stopifnot(all(dims == udim))
    }

    stopifnot(dim(img)[3] == nrow(run_df))
    stopifnot(is.na(dim(img)[4]))
    img = rescale_img(img)
    writenii(img, outfile)
    
    
    ss = CT_Skull_Strip(
      outfile, 
      outfile = ss_file,
      maskfile = maskfile,
      keepmask = TRUE)
    
  }
  
  if (all(file.exists(c(ss_file, maskfile, outfile)))) {
    
    if (!file.exists(pngfile)) {
      
      # ss = readnii(ss_file)
      img = readnii(outfile)
      mask = readnii(maskfile)
      
      png(pngfile, res = 300, units = "in", height = 7, width = 7)
      col.y = "#FF000080"
      img = window_img(img)
      ortho2(img, mask, col.y = col.y)
      dev.off()
    }
  }
}
