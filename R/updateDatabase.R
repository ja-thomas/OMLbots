updateDatabase = function(db, tag = "mlrRandomBotV1") {
  
  local.runs = tbl(db, "run.table")
  
  all.runs = listOMLRunEvaluations(tag = tag)
  new.runs = setdiff(as.data.frame(local.runs)$run.id, all.runs$run.id)
  
  db_insert_into(db$con, table = "run.table", new.runs)
}