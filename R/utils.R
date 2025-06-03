get_fold = function(default = 1L) {
  ifold = as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))
  if (all(is.na(ifold))) {
    ifold = default
  }
  ifold
}
