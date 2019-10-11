file_exists = function(...) {
  x = list(...)
  x = unlist(...)
  x = unname(x)
  x = tolower(x)
  df = data.frame(x = x, 
                  bn = basename(x),
                  dn = dirname(x),
                  index = 1:length(x),
                  stringsAsFactors = FALSE)
  udn = unique(df$dn)
  res = lapply(udn, function(path) {
    bn = list.files(path, recursive = FALSE, full.names = FALSE, all.files = TRUE)
    bn = tolower(bn)
    data.frame(dn = path, bn = bn, exists = TRUE, stringsAsFactors = FALSE)
  })
  res = do.call(rbind, res)
  lj = merge(df, res, all.x = TRUE, sort = FALSE)
  lj$exists[is.na(lj$exists)] = FALSE
  lj = lj[ order(lj$index), ]
  return(lj$exists)
}
