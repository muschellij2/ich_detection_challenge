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
