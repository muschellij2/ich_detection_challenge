rm(list = ls())
library(dcm2niir)
library(divest)
library(neurobase)
library(extrantsr)
library(ichseg)
library(dplyr)
library(tidyr)
library(fs)
library(dcmtk)
library(fslr)
setwd(here::here())

source("code/file_exists.R")
add_instance_number = TRUE

stage_number = 2
pre = ifelse(stage_number == 1, "", "stage2_")

tmp = sapply(c("ss", "mask", "nifti"), dir.create, 
             showWarnings = FALSE)

n_folds = 200

df = readr::read_rds("wide_headers_with_folds_outcomes.rds")
all_df = df

df = df %>%
  unite(col = label, sep = "",          
        any,
        epidural,
        intraparenchymal,
        intraventricular,
        subarachnoid,
        subdural, na.rm = TRUE) 
df = df %>% 
  mutate(label = ifelse(label == "NANANANANANA", NA_character_, label))
# use integer, then get to get back sprintf("%06.0f", 0)
df = df %>% 
  mutate(label = as.integer(label))

df = all_df
# 7646
# ID_02c48e85-ID_bd2131d216 
ifold = as.numeric(Sys.getenv("SGE_TASK_ID"))
if (is.na(ifold)) {
  ifold = 1
}

df = df[ df$fold == ifold,]

# x = list.files(pattern = ".nii.gz", path = "mask")
# all_files = list.files(pattern = ".nii.gz", path = "nifti")
# 
# no_data = all_files[ !basename(all_files) %in% basename(x)]
# 
# df = df[ basename(df$outfile) %in% no_data,]
# uids = unique(df$index)

# uids = c(16459, 16786, 19075)

uids = unique(df$index)
iid = uids[1]
# iid = 9708
# iid = 102
# iid = 219
for (iid in uids) {
  
  print(iid)
  run_df = df[ df$index == iid, ]
  
  
  ss_file = unique(run_df$ss_file)
  maskfile = unique(run_df$maskfile)
  out_maskfile = sub("[.]nii", "_Mask.nii", ss_file)
  
  # maskfile = sub("[.]nii", "_Mask.nii", ss_file)
  outfile = unique(run_df$outfile)
  
  reduced_file = file.path("reduced", basename(outfile))
  padded_file = file.path("reduced", 
                          sub("[.]", "_512.", basename(outfile)))
  reduced_maskfile = file.path("reduced", basename(out_maskfile))
  padded_maskfile = sub("[.]nii", "_Mask.nii", padded_file)
  
  reduced_rds = file.path("reduced", 
                          paste0(nii.stub(outfile, bn = TRUE), 
                                 ".rds"))
  
  
  n4_file = file.path("n4", basename(outfile))
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
    mutate(n_index = seq(n()),
           total_slices = n()) %>% 
    ungroup()
  duplicated_data = FALSE
  if (!all(run_df$n_index == 1)) {
    # we need to fix this
    # need slice_number then
    warning("Duplicated data!")
    duplicated_data = TRUE
  }
  all_run_df = run_df
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
  all_files = c(ss_file, maskfile, outfile, out_maskfile, robust_maskfile)
  if (all(file.exists(all_files))) {
    all_d = sapply(all_files, d3)
  }
  all_files = setdiff(all_files, robust_maskfile)
  
  if (!all(file.exists(all_files))) {
    
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
    zdim = voxdim(img)[3]
    
    if (zdim[1]*nsli(img) < 100) {
      zdim = max(diff(run_df$z))
      diffs = round(diff(run_df$z), 2)
      diffs = diffs[diffs > 0.35]
      zdim = sort(table(diffs), decreasing = TRUE)
      zdim = as.numeric(names(zdim))
      # should probably do this:
      # due to "ID_02c48e85-ID_bd2131d216
      # 6512 - weird
      # "ID_4b4643db-ID_99ef75869d"
      pixdim(img)[4] = zdim[1]
    }
    
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
  
  if (!all(file.exists(ss_robust_file, robust_maskfile)) & 
      file.exists(outfile)) {
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
  
  # if (all(file.exists(ss_robust_file, robust_maskfile)) & 
  #     !file.exists(n4_file)) {
  #   # val = 1024
  #   img = readnii(outfile)
  #   robust_mask = readnii(robust_maskfile)
  #   img = mask_img(img, robust_mask)
  #   img_range = (3071 - (-1024))
  #   img = (img - (-1024)) / img_range
  #   bc = bias_correct(
  #     file = img, 
  #     mask = robust_maskfile,
  #     correction = "N4")
  #   bc = bc * img_range + -1024
  #   bc = mask_img(bc, robust_mask)
  #   writenii(bc, n4_file)
  #   rm(bc)
  # }
  
  if (all(file.exists(ss_robust_file, robust_maskfile)) & 
      !all(file.exists(reduced_file, padded_file, 
                       reduced_rds, padded_maskfile))) {
    # val = 1024
    robust_mask = readnii(robust_maskfile)
    inds = getEmptyImageDimensions(robust_mask)
    # keep z fixed so keep with instance_numbers
    inds[[3]] = seq(1, dim(robust_mask)[3])
    readr::write_rds(inds, reduced_rds)
    
    robust_mask = applyEmptyImageDimensions(robust_mask, inds = inds)
    writenii(robust_mask, reduced_maskfile)
    # rm(robust_mask)
    ss = readnii(ss_robust_file)
    ss = applyEmptyImageDimensions(ss, inds = inds)
    writenii(ss, reduced_file)
    
    d_new = dim(robust_mask)[1:2]
    stopifnot(all(d_new <= 512))
    kdim = rep(0, 3)
    if (!all(d_new == 512)) {
      kdim = c((512 - d_new)/2, 0)
      robust_mask = zero_pad(robust_mask, kdim = kdim,
                             pad_value = 0)
      ss = zero_pad(ss, kdim = kdim,
                    pad_value = -1024)    
      stopifnot(all(dim(robust_mask)[1:2] == 512))
    }
    writenii(ss, padded_file)
    writenii(robust_mask, padded_maskfile)
  }  
  
  out_type = "png"
  compression = ifelse(out_type == "tiff", "LZW", "none")
  run_df = run_df %>% 
    arrange(instance_number)
  tiff_files = file.path(paste0(out_type, "_512"),
                         sub(".dcm", 
                             paste0(".", out_type), 
                             basename(run_df$file)))
  if (!all(file.exists(tiff_files))) {
    ss = readnii(padded_file)
    # no 255
    ss = (ss - (-1024)) / (3071 - (-1024)) 
    imgs = apply(ss, 3, function(x) list(EBImage::as.Image(x)))
    imgs = lapply(imgs, function(x) x[[1]])
    mapply(function(img, file) {
      EBImage::writeImage(img, file, compression = compression)
    }, imgs, tiff_files)
  }
  
  
  run_df = run_df %>% 
    arrange(instance_number)
  for (isize in c(128, 256)) {
    size_dir = paste0(out_type, "_", isize)
    if (!dir.exists(size_dir)) {
      dir.create(size_dir)
    }
    tiff_files = file.path(size_dir,
                           sub(".dcm", 
                               paste0(".", out_type), 
                               basename(run_df$file)))
    if (!all(file.exists(tiff_files))) {
      ss = readnii(padded_file)
      ss = (ss - (-1024)) / (3071 - (-1024)) 
      ss = extrantsr::resample_image(
        ss, 
        parameters = c(isize, isize, dim(ss)[3]),
        interpolator = "linear",
        parameter_type = "voxels")
      d_new = dim(ss)[1:2]
      stopifnot(all(d_new == isize))
      # no 255
      imgs = apply(ss, 3, function(x) list(EBImage::as.Image(x)))
      imgs = lapply(imgs, function(x) x[[1]])
      mapply(function(img, file) {
        EBImage::writeImage(
          img, file, 
          compression = compression)
      }, imgs, tiff_files)
    }
    
    all_run_df = all_run_df %>% 
      mutate(tiff_file = file.path(
        size_dir,
        sub(".dcm",                             
            paste0(".", out_type), 
            basename(file)))    
      )
    
    if (duplicated_data) {
      if (!all(file.exists(all_run_df$tiff_file))) {
        wide = all_run_df %>% 
          filter(total_slices > 1) %>% 
          select(tiff_file, n_index, x, y, z) %>% 
          tidyr::spread(key = n_index, value = tiff_file) %>% 
          unite(x,y,z, col = xyz) %>% 
          rename(original = `1`)
        wide = split(wide, wide$xyz)
        xx  = lapply(wide, function(r) {
          orig = r$original
          r = r %>% 
            select(-xyz, -original)
          r = unlist(r)
          orig = rep(orig, length = length(r))
          file.copy(orig, r, overwrite = TRUE)
        })
      }
    }
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

