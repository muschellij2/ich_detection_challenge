library(dcmtk)
library(dplyr)
library(readr)
source(here::here("R/utils.R"))

filename = here::here("data", "dicom_filenames.rds")
df = readr::read_rds(filename)

iid = 1

ifold = get_fold(default = NA_real_)
if (all(is.na(ifold))) {
  ifold = sort(unique(df$fold))
}
df = df %>%
  filter(fold %in% ifold)

print(nrow(df))

for (iid in seq(nrow(df))) {
  print(iid)
  idf = df[iid, ]
  if (!file.exists(idf$hdr)) {
    res = dcmtk::read_dicom_header(file = idf$file)
    res = dcmtk::wide_hdr(res)
    readr::write_rds(res, idf$hdr, compress = "xz")
  } 
  # else {
  #   res = readr::read_rds(idf$hdr)
  # }
}
