library(oro.dicom)
library(dcmtk)
library(neurobase)
library(readr)

df = read_rds("wide_headers_with_folds.rds")

x = df[ df$index == 1,]

img = readnii(unique(x$ss_file))

inst = floor(dim(img)[3]/2)
test_dcm = x[ x$instance_number == inst, ]

replace_pixel_data = function(dicom, mat, rescale_data = TRUE, verbose = TRUE) {
  stopifnot(is.matrix(mat))
  tfile = tempfile(fileext = ".dcm")
  file.copy(dicom, tfile)
  
  hdr = readDICOMFile(tfile)
  d_orig = dim(hdr$img)
  stopifnot(all(dim(mat) == d_orig))
  header = read_dicom_header(tfile, verbose = verbose)
  header = wide_hdr(header)

  if (rescale_data) {
    header$RescaleIntercept = readr::parse_number(header$RescaleIntercept)
    header$RescaleSlope = readr::parse_number(header$RescaleSlope)
    stopifnot(!is.na( header$RescaleSlope),
              !is.na( header$RescaleIntercept))
    mat = (mat - header$RescaleIntercept) * header$RescaleSlope 
  }
  mat = c(mat)
  mat = as.integer(mat)
  rawfile = tempfile(fileext = ".raw")
  writeBin(mat, rawfile, size = 2L)
  opts = paste0('-if "PixelData=', rawfile, '" ')
  result = dcmodify(file = tfile, frontopts = opts, verbose = verbose)
  if (result != 0) {
    warning("bad result maybe")
  }
  return(tfile)
}

dicom = tempfile(fileext = ".dcm")
file.copy(test_dcm$file, dicom)

mat = as.matrix(img[,,inst])
image(mat)
res = replace_pixel_data(dicom, mat)

hdr = readDICOMFile(res)
image(hdr$img)

