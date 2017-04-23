#' Initialize database
#' @param path path of database
#' @param overwrite overwrite existing database
#' @return database
initializeLocalDatabase = function(path = ".", overwrite = FALSE) {
  
  db = paste0(path, "/mlrRandomBotDatabase.db")
  
  if (file.exists(db) & overwrite) {
    unlink(db, force = TRUE)
  } 
  
  src = src_sqlite(db, create = !file.exists(db))
  
  if(!db_has_table(src$con, "run.table")){
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
  }
  if(!db_has_table(src$con, "hyperpar.table")){
    hyperpar.table = data.frame(run.id = integer(0L), 
      hyperpar.name = character(0L), 
      hyperpar.value = character(0L),
      stringsAsFactors = FALSE)
    copy_to(src, hyperpar.table, temporary = FALSE)
  }
  if(!db_has_table(src$con, "meta.table")){
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
  }
  if(!db_has_table(src$con, "runtime.table")){
    runtime.table = data.frame(run.id = integer(0L),
      run.time = numeric(0L),
      sci.mark = numeric(0L),
      stringsAsFactors = FALSE)
    copy_to(src, runtime.table, temporary = FALSE)
  }
  
  return(src)
}

#' Save run results to db
updateRunTable = function(run.tag, local.db){
  run.ids = local.db %>% tbl(sql("SELECT DISTINCT [run.id] FROM [run.table]")) %>% collect(n = Inf)
  df = getRunTable(run.tag = run.tag, excl.run.ids = run.ids$run.id)
  if(!is.null(df)){
    db_insert_into(local.db$con, "run.table", df)
  }
}

#' Save hyperparameters for run to db
updateHyperparTable = function(run.tag, local.db){
  run.ids = local.db %>% tbl(sql("SELECT DISTINCT [run.id] FROM [hyperpar.table]")) %>% collect(n = Inf) 
  df = getHyperparTable(run.tag = run.tag, excl.run.ids = run.ids$run.id)
  if(!is.null(df)){
    db_insert_into(local.db$con, "hyperpar.table", df)
  }
}

#' Save meta data for task to db
updateMetaTable = function(task.tag, local.db){
  task.ids = local.db %>% tbl(sql("SELECT DISTINCT [task.id] FROM [meta.table]")) %>% collect(n = Inf) 
  df = getMetaFeaturesTable(task.tag = task.tag)
  df = df[df$task.id %in% setdiff(df$task.id, task.ids),]
  if(!is.null(df)){
    db_insert_into(local.db$con, "meta.table", df)
  }
}

#' Save user times to db
updateRunTimeTable = function(local.db){
  qry_sql = paste0("SELECT DISTINCT a.[run.id] FROM [run.table] As a ",
    "LEFT JOIN [runtime.table] As b ON a.[run.id] = b.[run.id] ",
    "WHERE b.[run.id] IS NULL")
  run.ids = local.db %>% tbl(sql(qry_sql)) %>% collect(n = Inf)
  run.chunks = split(run.ids$run.id, ceiling(seq_along(run.ids$run.id)/100))
  
  run.chunks = run.chunks[1:10] #just for testing: can be switched off
  
  for(i in run.chunks){
    df = getRunTime(i)
    if(!is.null(df)){
      db_insert_into(local.db$con, "runtime.table", df)
    }
    df = NULL
  }
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
  updateRunTimeTable(local.db)
}





