# initialize a local sqlite database to store results for vizualization
initializeLocalDatabase = function(path = ".", overwrite = FALSE) {
  path = paste0(path, "/mlrRandomBotDatabase.db")
  if (file.exists(path)) {
    if(overwrite)
      unlink(path)
    else
      stop("Database already exists and overwrite is set to FALSE")
  }
  
  src = src_sqlite(path, create = TRUE)
  
  run.table = data.frame(run.id = integer(0), task.id = integer(0), flow.id = integer(0),
    flow.name = character(0), measure.name = character(0), measure.value = numeric(0))
  run.table.db = copy_to(src, run.table, temporary = FALSE)
  hyperpar.table = data.frame(run.id = integer(0), hyperpar.name = character(0), 
    hyperpar.value = character(0))
  hyperpar.table.db = copy_to(src, hyperpar.table, temporary = FALSE)
  
  return(src)
}