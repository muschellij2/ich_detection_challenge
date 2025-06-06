library(dcmtk)
library(dplyr)
library(readr)
library(here)
library(purrr)
library(tidyr)

df_dir = expand_grid(
  # stage_number = 1:2,
  stage_number = 2,
  group = c("train", "test")
) %>% 
  mutate(
    dir = here::here("data", paste0("stage_", stage_number, "_", group, "_images"))
  ) %>% 
  filter(dir.exists(dir))
print(df_dir)

paths = df_dir$dir
df = map_df(paths, function(path) {
  files = list.files(path, full.names = TRUE, recursive = TRUE, pattern = "\\.dcm$")
  tibble(
    file = files,
    fname = basename(files),
    dir = path
  )
}, .progress = TRUE)

df = df %>% 
  dplyr::left_join(df_dir)

df = df %>%
  mutate(id = sub("\\.dcm$", "", fname)) 
stopifnot(anyDuplicated(df$id) == 0)

n_folds = 200
df = df %>% 
  mutate(
    hdr = here::here("data", "header", paste0(id, ".rds")),
    fold = (0:(nrow(df)-1)) %% n_folds + 1
  )

fs::dir_create(here::here("data", "header"))
outfile = here::here("data", "dicom_filenames.rds")
readr::write_rds(df, outfile)


