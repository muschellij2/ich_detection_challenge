rm(list = ls())
library(dcm2niir)
library(divest)
library(neurobase)
library(ichseg)
library(dplyr)
library(fs)
library(dcmtk)
library(fslr)
setwd(here::here())

add_instance_number = TRUE

sub_bracket = function(x) {
  x = sub("^\\[", "", x)
  x = sub("\\]$", "", x)
  x = trimws(x)
}

head_size = function(x) {
  res = fslr::fslhd(x, verbose = FALSE)
  hdr = fslr::fslhd.parse(res)
  pdim = as.numeric(hdr["pixdim3",])
  nslices = as.numeric( hdr["dim3",])
  pdim*nslices
}

tmp = sapply(c("ss", "mask", "nifti"), dir.create, 
             showWarnings = FALSE)

n_folds = 200

df = readr::read_rds("wide_headers_with_folds.rds")
all_df = df

df = all_df
# 7646
# ID_02c48e85-ID_bd2131d216 
ifold = as.numeric(Sys.getenv("SGE_TASK_ID"))
if (is.na(ifold)) {
  ifold = 51
}

df = df[ df$fold == ifold,]

# x = list.files(pattern = ".nii.gz", path = "mask")
# all_files = list.files(pattern = ".nii.gz", path = "nifti")
# 
# no_data = all_files[ !basename(all_files) %in% basename(x)]
# 
# df = df[ basename(df$outfile) %in% no_data,]
# uids = unique(df$index)



uids = unique(df$index)
iid = uids[1]
# iid = 15977
# iid =102
#iid = 219
for (iid in uids) {
  
  print(iid)
  run_df = df[ df$index == iid, ]
  
  
  
  ss_file = unique(run_df$ss_file)
  maskfile = unique(run_df$maskfile)
  out_maskfile = sub("[.]nii", "_Mask.nii", ss_file)
  
  # maskfile = sub("[.]nii", "_Mask.nii", ss_file)
  outfile = unique(run_df$outfile)
  alt_outfile = unique(run_df$alt_outfile)
  alt_ss_file = sub("ss/", "eq_ss/", ss_file)
  alt_ss_maskfile = sub("[.]nii", "_Mask.nii", alt_ss_file)
  pngfile = unique(run_df$pngfile)
  robust_pngfile = sub("png/", "png_robust/", pngfile)
  ss_robust_file = unique(run_df$ss_robust_file)
  robust_maskfile = sub("[.]nii", "_Mask.nii", ss_robust_file)
  stopifnot(length(ss_file) == 1,
            length(maskfile) == 1,
            length(ss_robust_file) == 1,
            length(outfile) == 1)
  
  
  
  # sorting is below this, can do x y z here
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
  
  # check head size for small things need to rerun
  if (file.exists(outfile)) {
    # hs = head_size(outfile)
    # if (hs < 100) {
    #   file.remove(c(ss_file, maskfile, outfile, pngfile, 
    #                 robust_pngfile,
    #                 ss_robust_file,
    #                 robust_maskfile))
    # }
  }
  
  if (!all(file.exists(c(ss_file, maskfile, outfile, out_maskfile)))) {
    
    nu = function(x) length(unique(x))
    stopifnot(nu(run_df$PatientID) == 1,
              nu(run_df$StudyInstanceUID) == 1,
              nu(run_df$SeriesInstanceUID) == 1
    )
    
    run_df = run_df %>% 
      select(PatientID, StudyInstanceUID, SeriesInstanceUID, 
             x, y, z, everything()) %>% 
      arrange(PatientID, StudyInstanceUID, SeriesInstanceUID, z) %>% 
      ungroup() %>% 
      mutate(instance_number = seq(n()))
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
      # id 805 sauce
      # "ID_08f09333-ID_4fd74dabb2" is even worse
      x = divest::scanDicom(path = tdir, forceStack = TRUE)
      stack_data = TRUE
    }
    stopifnot(nrow(x) == 1)
    paths = attributes(x)$paths[[1]]
    
    stopifnot(all(basename(paths) == basename(run_df$file)))
    
    tmp_paths = file.path(tdir, basename(run_df$file))
    if (add_instance_number) {
      ind = seq_along(tmp_paths)
      add_instance = function(file, index) {
        dcmtk::dcmodify(
          file = file,
          frontopts = paste0('-i "(0020,0013)=', index, '"'),
          verbose = FALSE
        )
        hdr = read_dicom_header(file, verbose = FALSE)
        new_inst = as.numeric(sub_bracket(hdr$value[hdr$name == "InstanceNumber"]))
        stopifnot(new_inst == index)
        print(new_inst)
        bakfile = paste0(file, ".bak")
        if (file.exists(bakfile)) {
          file.remove(bakfile)
        }
      }
      res = mapply(add_instance, tmp_paths, ind)
    }
    
    
    d_res = dcm2nii(basedir = tdir,
                    opts = paste0(
                      "-9 ",
                      " -v y ",
                      if (stack_data) "-m y",
                      " -z i -f %p_%t_%s"))
    nii = d_res$nii_after
    # need to remove these
    alt = grepl("(Tilt|Eq)", nii)
    if (any(alt)) {
      alt_nii = nii[alt]
      stopifnot(length(alt_nii) == 1)
      gf = fslr::getForms(alt_nii)
      if (!all(gf$ssor != gf$sqor)) {
        if (all(gf$sqor == c("RL", "PA", "IS"))) {
          alt_img = fslorient(alt_nii, opts = "-copyqform2sform")
        }
      }
      alt_img = readnii(alt_nii[1])
      alt_img = rescale_img(alt_img)
    }
    nii = nii[ !alt]
    stopifnot(length(nii) == 1)
    img = readnii(nii[1])
    zdim = max(diff(run_df$z))
    diffs = round(diff(run_df$z), 2)
    diffs = diffs[diffs > 0.3]
    zdim = sort(table(diffs), decreasing = TRUE)
    zdim = as.numeric(names(zdim))
    # should probably do this:
    # due to "ID_02c48e85-ID_bd2131d216
    # 6512 - weird
    # "ID_4b4643db-ID_99ef75869d"
    pixdim(img)[4] = zdim[1]
    if (zdim[1]*nsli(img) < 100) {
      warning("probably flat head!! Too small z-dimensions")
    }
    
    # ID_5d81e0ab-ID_e32965796b sent to dcm2niix rorden
    # ID of ID_5d81e0ab-ID_e32965796b is 8011
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
    
    if (any(alt)) {
      writenii(alt_img, alt_outfile)
      rm(alt_img)
    }
    
    # may need robust:
    # ID_173a2d6e-ID_4e2089d46f.nii.gz
    ss = CT_Skull_Strip(
      outfile, 
      outfile = ss_file,
      maskfile = maskfile,
      keepmask = TRUE)
    out_maskfile = sub("[.]nii", "_Mask.nii", ss_file)
    file.copy(maskfile, out_maskfile, overwrite = TRUE)
    rm(ss)
  }
  
  if (!file.exists(ss_robust_file) & file.exists(outfile)) {
    val = 1024
    img = readnii(outfile) + val
    tfile = tempfile(fileext = ".nii.gz")
    ss = CT_Skull_Strip_smooth(
      img, 
      outfile = tfile,
      lthresh = 0 + val,
      uthresh = 100 + val,
      mask_to_background = FALSE)
    rm(ss)
    rbmask = sub("[.]nii", "_Mask.nii", tfile)
    mask = readnii(rbmask)
    xss = mask_img(img, mask) - val
    writenii(mask, robust_maskfile)
    rm(mask)
    writenii(xss, ss_robust_file)
    rm(xss)
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
      rm(img);
      rm(mask)
    }
  }
  
  if (all(file.exists(c(ss_robust_file, robust_maskfile, outfile)))) {
    
    if (!file.exists(robust_pngfile)) {
      
      # ss = readnii(ss_file)
      img = readnii(outfile)
      mask = readnii(robust_maskfile)
      
      png(robust_pngfile, res = 300, units = "in", height = 7, width = 7)
      col.y = "#FF000080"
      img = window_img(img)
      ortho2(img, mask, col.y = col.y)
      dev.off()
      rm(img);
      rm(mask)
    }
  }  
  
  if (!(file.exists(alt_ss_file) & file.exists(alt_ss_maskfile) ) &
      file.exists(alt_outfile)) {
    # print("no alt stuff")
    
    val = 1024
    img = readnii(alt_outfile) + val
    tfile = tempfile(fileext = ".nii.gz")
    ss = CT_Skull_Strip_smooth(
      img,
      outfile = tfile,
      lthresh = 0 + val,
      uthresh = 100 + val,
      mask_to_background = FALSE)
    rm(ss)
    rbmask = sub("[.]nii", "_Mask.nii", tfile)
    mask = readnii(rbmask)
    xss = mask_img(img, mask) - val
    writenii(mask, alt_ss_maskfile)
    rm(mask)
    writenii(xss, alt_ss_file)
    rm(xss)
  }
  
}

