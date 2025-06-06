get_fold = function(default = 1L) {
  ifold = as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))
  if (all(is.na(ifold))) {
    ifold = default
  }
  ifold
}


remove_brackets = function(x) {
  x = sub("^\\[", "", x)
  x = sub("\\]$", "", x)
}

remove_forward_slash = function(x) {
  x = sub("^/", "", x)
  x = sub("/$", "", x)
}

remove_back_slash = function(x) {
  x = sub("^\\\\", "", x)
  x = sub("\\\\$", "", x)
}

gsub_under = function(x, replacement = "-") {
  gsub(x, pattern = "_", replacement = replacement)
}


copy_dcm_files = function(df) {
  new_fname = new_file = NULL
  rm(list = c("new_fname", "new_file"))
  files = df$file
  stopifnot(anyDuplicated(basename(files)) == 0)
  tdir = tempfile()
  fs::dir_create(tdir)
  file_df = dplyr::tibble(
    file = files,
    new_fname = janitor::make_clean_names(tolower(basename(files)))
  ) %>%
    mutate(
      new_fname = sub("_dcm$", ".dcm", new_fname),
      new_file = file.path(tdir, new_fname)
    )
  file.copy(file_df$file, file_df$new_file)
  file_df %>%
    select(-any_of("new_file"))
  list(
    outdir = tdir,
    file_df = file_df
  )
}
