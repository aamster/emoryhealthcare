import_package('data.table', attach = T)

rename_columns = function(data, cols) {
  for (col in cols) {
    if (col$old %in% names(data)) {
      setnames(data, col$old, col$new)
    } else {
      similar = str_detect(names(data), paste(substr(col$old, 1, nchar(col$old)-1), '.{1}$', sep=''))
      if (any(similar)) {
        col$old = names(data)[similar][1]
        setnames(data, col$old, col$new)
      }
    }
  }
}

read_and_preprocess_data = function(path, cols, rename=T) {
  data = fread(path)
  if (rename) {
    rename_columns(data, cols)
  }
  cols = unique(c('SEQN',
                  sapply(cols, function(x)
                    x$new)))
  data = data[, .SD, .SDcols = cols]
  return (data)
}