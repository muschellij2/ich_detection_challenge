library(neurobase)
library(ichseg)
library(dcm2niir)
library(extrantsr)
library(lungct)
setwd("~/Downloads")

dcms = list.files(pattern = "ID_.*.dcm")
outfiles = sub(".dcm", ".nii.gz", dcms)
body_outfiles = sub(".dcm", "_body.nii.gz", dcms)

i = 5
for (i in seq_along(dcms)) {
  print(i)
  dcm = dcms[i]
  outfile = outfiles[i]
  if (!file.exists(outfile)) {
    res = ct_dcm2nii(files = dcm, drop_dim = FALSE)
    print(class(res))
    writenii(res, outfile, drop_dim = FALSE)
  } else {
    res = readnii(outfile, drop_dim = FALSE)
  }

  body_outfile = body_outfiles[i]
  if (!file.exists(body_outfile)) {
    x = lungct::segment_human(res, drop_dim = FALSE,
                              smooth = FALSE,
                              lthresh = -400)
    write_nifti(x$body, body_outfile)
    body = check_nifti(x$body, drop_dim = FALSE)
  } else {
    body = readnii(body_outfile, drop_dim = FALSE)
  }
  pngname = sub(".dcm", ".png", dcm)
  if (!file.exists(pngname)) {
    png(pngname, res = 300, units = "in", height = 7, width = 7)
    ortho2(res, body)
    dev.off()
  }
}
