#' Initialize database
#' @param path path of database
#' @param overwrite overwrite existing database
#' @return database
initializeLocalDatabase = function(path = ".", overwrite = FALSE) {
  
  db = paste0(path, "/mlrRandomBotDatabase.db")
  
  if (file.exists(db) & overwrite) {
    unlink(db, force = TRUE)
  } 
  
  if (!file.exists(db)){
    src = src_sqlite(db, create = TRUE)
    
    run.table = data.frame(run.id = integer(0L), 
      task.id = integer(0L), 
      setup.id = integer(0L),
      flow.id = integer(0L),
      flow.name = character(0L),
      flow.version = character(0L),
      flow.source = character(0L),
      learner.name = character(0L),
      data.name = character(0L),
      upload.time = character(0L),
      measure.name = character(0L), 
      measure.value = numeric(0L),
      stringsAsFactors = FALSE)
    copy_to(src, run.table, temporary = FALSE)
    
    hyperpar.table = data.frame(run.id = integer(0L), 
      hyperpar.name = character(0L), 
      hyperpar.value = character(0L),
      stringsAsFactors = FALSE)
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
      number.of.symbolic.features = integer(0L),
      stringsAsFactors = FALSE)
    copy_to(src, meta.table, temporary = FALSE)
    
  } else {
    src = src_sqlite(db)
  }
  
  return(src)
}

updateRunTable = function(run.tag, local.db){
  run.ids <- collect(tbl(local.db, sql("SELECT DISTINCT [run.id] FROM [run.table]")))
  df = getRunTable(run.tag = run.tag, excl.run.ids = run.ids$run.id)
  if(!is.null(df)){
    db_insert_into(local.db$con, "run.table", df)
  }
}

updateHyperparTable = function(run.tag, local.db){
  run.ids <- collect(tbl(local.db, sql("SELECT DISTINCT [run.id] FROM [hyperpar.table]")))
  df = getHyperparTable(run.tag = run.tag)
  db_insert_into(local.db$con, "hyperpar.table", df)
}

updateMetaTable = function(task.tag, local.db){
  df = getMetaFeaturesTable(task.tag = task.tag)
  db_insert_into(local.db$con, "meta.table", df)
}

#' Check each tables of the local database for update requirement
#' @param path path of database
#' @param run.tag tag for OMLRun
#' @param task.tag tag for OMLTask
#' @return database
updateLocalDatabase = function(path = ".", run.tag = "mlrRandomBot", task.tag = "study_14") {
  
  local.db = initializeLocalDatabase(path = path)
  updateRunTable(run.tag = run.tag, local.db)
  #updateHyperparTable(run.tag = run.tag, local.db)
  #updateMetaTable(task.tag = task.tag, local.db)
}





