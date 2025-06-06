library(dplyr)
source(here::here("R/utils.R"))
setwd(here::here("original_data"))
df = readr::read_rds("zip_files.rds")

ifold = get_fold(default = NA_real_)
if (all(is.na(ifold))) {
  ifold = sort(unique(df$fold))
}
df = df %>%
  filter(fold %in% ifold)

print(nrow(df))


# for (iid in seq(nrow(df))) {
  # print(iid)
  # idf = df[iid, ]
  df = df %>% 
    filter(!file.exists(Name))
  if (nrow(df) > 0) {
  # if (!file.exists(idf$Name)) {
    unzip("rsna-intracranial-hemorrhage-detection-stage-2.zip", files = df$Name)
  # } 
  }
  # else {
  #   res = readr::read_rds(idf$hdr)
  # }
# }

