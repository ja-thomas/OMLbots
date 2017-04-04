#' Initialize database
#' @param path path of database
#' @param overwrite overwrite existing database
#' @return database
initializeLocalDatabase = function(path = ".", overwrite = FALSE) {
  
  path = paste0(path, "/mlrRandomBotDatabase.db")
  
  if (file.exists(path) & overwrite) {
    unlink(path)
  } 
  
  if (!file.exists(path)){
    src = src_sqlite(path, create = TRUE)
    
    run.table = data.frame(run.id = integer(0L), 
      task.id = integer(0L), 
      flow.id = integer(0L),
      flow.name = character(0L), 
      measure.name = character(0L), 
      measure.value = numeric(0L))
    copy_to(src, run.table, temporary = FALSE)
    
    hyperpar.table = data.frame(run.id = integer(0L), 
      hyperpar.name = character(0L), 
      hyperpar.value = character(0L))
    copy_to(src, hyperpar.table, temporary = FALSE)
    
    meta.table = data.frame(task.id = integer(0L),
      data.id = integer(0L),
      name = character(0L),
      majority.class.size = integer(0L),
      max.nominal.att.distinct.values = integer(0L),
      minority.class.size = integer(0L),
      number.of.classes = integer(0L),
      number.of.features = integer(0L),
      number.of.instances = integer(0L),
      number.of.instances.with.missing.values = integer(0L),
      number.of.missing.values = integer(0L),
      number.of.numeric.features = integer(0L),
      number.of.symbolic.features = integer(0L))
    copy_to(src, meta.table, temporary = FALSE)
    
  } else {
    src = src_sqlite(path)
  }
  
  return(src)
}

updateRunTable = function(run.tag, local.db){
  df = getRunTable(run.tag = run.tag)
  
}

updateHyperparTable = function(run.tag, local.db){
  df = getHyperparTable(run.tag = run.tag)
  
}

updateMetaTable = function(task.tag, local.db){
  df = getMetaFeaturesTable(task.tag = task.tag)
  
}

#' Check each tables of the local database for update requirement
#' @param path path of database
#' @param run.tag tag for OMLRun
#' @param task.tag tag for OMLTask
#' @return database
updateLocalDatabase = function(path = ".", run.tag = "mlrRandomBot", task.tag = "study_14") {
  
  local.db = initializeLocalDatabase(path = path)
  updateRunTable(run.tag = run.tag, local.db)
  updateHyperparTable(run.tag = run.tag, local.db)
  updateMetaTable(task.tag = task.tag, local.db)
  
  
  
  local.runs = tbl(db, "run.table")
  
  all.runs = listOMLRunEvaluations(tag = tag)
  new.runs = setdiff(as.data.frame(local.runs)$run.id, all.runs$run.id)
  
  db_insert_into(db$con, table = "run.table", new.runs)
}





