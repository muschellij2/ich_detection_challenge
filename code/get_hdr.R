library(dcmtk)
library(dplyr)
library(readr)
setwd(here::here())

# df = readr::read_rds("stage_1_train_data.rds")

ifold = as.numeric(Sys.getenv("SGE_TASK_ID"))
if (is.na(ifold)) {
  ifold = 80
}

outfile = file.path("hdr", paste0("fold_", ifold, ".rds"))
wide_outfile = file.path("wide_hdr", paste0("fold_", ifold, ".rds"))

if (!all(file.exists(c(outfile, wide_outfile)))) {
  df = readr::read_rds("stage_1_data.rds")
  df = df[ df$fold == ifold,]
  
  df = df %>% 
    mutate(hdr = file.path("hdr", paste0(ID, ".rds")),
           wide_hdr = file.path("wide_hdr", paste0(ID, ".rds")))
  
  # df = df %>%
  #   filter(!file.exists(hdr))
  
  iid = 1
  
  print(nrow(df))
  results = wide_results = list(mode = "vector", length = nrow(df))
  
  for (iid in seq(nrow(df))) {
    print(iid)
    idf = df[iid, ]
    if (!file.exists(idf$hdr)) {
      res = dcmtk::read_dicom_header(file = idf$dcm)
      readr::write_rds(res, idf$hdr)
    } else {
      res = readr::read_rds(idf$hdr)
    }
    stopifnot(!is.null(res))
    results[[iid]] = res
    if (!file.exists(idf$wide_hdr)) {
      wide = dcmtk::wide_hdr(res)
      stopifnot(nrow(wide) == 1)
      readr::write_rds(wide, idf$wide_hdr)
    } else {
      wide = readr::read_rds(idf$wide_hdr)
    }
    wide_results[[iid]] = wide
    suppressWarnings(rm(list = c("res", "wide")))
  }
  
  wide = bind_rows(wide_results)
  readr::write_rds(wide, wide_outfile)
  rm(wide)
  rm(wide_results)
  
  res = bind_rows(results)
  readr::write_rds(res, outfile)
  
}
