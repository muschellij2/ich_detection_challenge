library(tibble)
library(dplyr)
library(ANTsRCore)
source(here::here("R/utils.R"))
outfile = here::here("data", "series_data.rds")
df = readr::read_rds(outfile)

adim = function(file) {
  ANTsRCore::AntsImageHeaderInfo(file)$dimensions[1:4]
}
info = purrr::map(df$file_nifti, ANTsRCore::AntsImageHeaderInfo, .progress = TRUE)
get_dim = function(x, n = 4) {
  x$dimensions[1:n]
}
results = purrr::map(info, get_dim, .progress = TRUE)

get_spacing = function(x, n = 4) {
  x$spacing[1:n]
}
spacing = purrr::map(info, get_spacing, .progress = TRUE)


mat = t(sapply(results, identity))
colnames(mat) = paste0("dim", 1:4)
dim_mat = as_tibble(mat)
if (all(is.na(dim_mat$dim4))) {
  dim_mat = dim_mat %>%
    select(-dim4)
}

mat = t(sapply(spacing, identity))
colnames(mat) = paste0("spacing_dim", 1:4)
spacing_mat = as_tibble(mat)
if (all(is.na(spacing_mat$spacing_dim4))) {
  spacing_mat = spacing_mat %>%
    select(-spacing_dim4)
}


out = df %>%
  select(file_nifti) %>% 
  bind_cols(dim_mat) %>% 
  bind_cols(spacing_mat)

readr::write_rds(out, here::here("data", "nifti_image_dimensions.rds"))

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

