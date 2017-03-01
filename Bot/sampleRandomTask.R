#' @return OML task
sampleRandomTask = function() {
  
  tasks = listOMLTasks(number.of.classes = 2L, data.tag = "study_14")
  task = tasks %>% 
    filter(estimation.procedure == "10-fold Crossvalidation", 
      format == "ARFF", status == "active", number.of.missing.values == 0) %>% 
    sample_n(1) %>% 
    select(task.id)
  
  return(task$task.id)
}
