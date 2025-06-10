library(tibble)
library(dplyr)
library(ANTsRCore)
source(here::here("R/utils.R"))
outfile = here::here("data", "series_data.rds")
df = readr::read_rds(outfile)

adim = function(file) {
  ANTsRCore::AntsImageHeaderInfo(file)$dimensions
}
results = purrr::map(df$file_nifti, adim, .progress = TRUE)

mat = t(sapply(results, identity))
colnames(mat) = paste0("dim", 1:3)
df = df %>%
  select(file_nifti) %>% 
  bind_cols(as_tibble(mat))

readr::write_rds(df, here::here("data", "nifti_image_dimensions.rds"))

# df = readr::read_rds(here::here("data", "nifti_image_dimensions.rds"))
# df512 = df %>%
#   filter(dim1 == 512, dim2 == 512)
# 
# df512 = df512 %>%
#   mutate(
#     ctbet = here::here(
#       "CT_BET/results_folder/unet_CT_SS_202564_16627/predictions",
#       basename(file_nifti)
#     ),
#     fe = file.exists(ctbet)
#   )

