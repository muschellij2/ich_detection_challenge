file_exists = function(..., verbose = TRUE) {
  x = list(...)
  x = unlist(...)
  x = unname(x)
  if (length(x) == 0) {
    return(logical(0))
  }
  x = tolower(x)
  df = data.frame(x = x, 
                  bn = basename(x),
                  dn = dirname(x),
                  index = 1:length(x),
                  stringsAsFactors = FALSE)
  udn = unique(df$dn)
  if (verbose) {
    msg = paste0("There are ", length(udn), " unique directories")
    message(msg)
  }
  res = lapply(udn, function(path) {
    if (verbose > 1) {
      msg = paste0("Listing ", path, " directory")
      message(msg)
    }
    bn = list.files(path, recursive = FALSE, full.names = FALSE, all.files = TRUE)
    exists = TRUE
    if (length(bn) == 0) {
      exists = NA
      bn = NA    
    }
    bn = tolower(bn)
    data.frame(dn = path, bn = bn, exists = exists, stringsAsFactors = FALSE)
  })
  if (verbose) {
    msg = paste0("Binding output")
    message(msg)
  }
  res = do.call(rbind, res)
  if (verbose) {
    msg = paste0("Merging data")
    message(msg)
  }  
  lj = merge(df, res, all.x = TRUE, sort = FALSE)
  lj$exists[is.na(lj$exists)] = FALSE
  lj = lj[ order(lj$index), ]
  return(lj$exists)
}


d3 = function(x) {
  x = fslr::fslval(x, keyword = "pixdim3", verbose = FALSE)
  as.numeric(x)
}


sub_bracket = function(x) {
  x = sub("^\\[", "", x)
  x = sub("\\]$", "", x)
  x = trimws(x)
}

head_size = function(x) {
  res = fslr::fslhd(x, verbose = FALSE)
  hdr = fslr::fslhd.parse(res)
  pdim = as.numeric(hdr["pixdim3",])
  nslices = as.numeric( hdr["dim3",])
  pdim*nslices
}



parse_number_no_na = function(x) {
  x[ x %in% c("-", "-", "N/A")] = NA
  na_x = is.na(x)
  x = readr::parse_number(x, na = "")
  bad = is.na(x) & !na_x 
  stopifnot(!any(bad))
  x
}

BinToDec <- function(x, n_slots = 6) {
  if (!is.character(x)) {
    stopifnot(length(x) == n_slots)
    stopifnot(all(x %in% c(0, 1)))
    x = paste(x, collapse = "")
  }
  nc = nchar(x)
  stopifnot(all(na.omit(nc) == n_slots))
  strtoi(x, base = 2)
}

DecToBin <- function(x, n_slots = 6) {
  x = as.integer(intToBits(x))[1:n_slots]
  paste(rev(x), collapse = "")
}
