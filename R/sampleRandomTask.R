# this draws a random binary classif OMLTask from study 14 with 10 fold CV and without missing values
# @return OML task
sampleRandomTask = function() {
  
  tasks = listOMLTasks(number.of.classes = 2L, number.of.missing.values = 0, 
    data.tag = "study_14", estimation.procedure == "10-fold Crossvalidation")
  print(sprintf("Found %i different task", nrow(tasks)))
  task = tasks %>% 
    filter(format == "ARFF", status == "active") %>% 
    sample_n(1) %>% 
    select(task.id, name)
  
  return(list(id = task$task.id, name = task$name))
}

# sample really simple and small datasets
sampleSimpleTask = function() {
  
  tasks = listOMLTasks(number.of.classes = 2L, number.of.instances = c(100L, 500L), 
    number.of.features = c(3L, 20L), number.of.missing.values = 0, estimation.procedure = "33% Holdout set")
  print(sprintf("Found %i different task", nrow(tasks)))
  task = tasks %>%
    filter(format == "ARFF", status == "active") %>% 
    sample_n(1) %>% 
    select(task.id, name)
  return(list(id = task$task.id, name = task$name))
  
}
