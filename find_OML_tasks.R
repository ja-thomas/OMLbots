library(OpenML)
library(dplyr)


tasks = listOMLTasks(number.of.classes = 2L, data.tag = "study_14")

ids = tasks %>% 
  filter(estimation.procedure == "10-fold Crossvalidation", format == "ARFF", 
    status == "active", evaluation.measures == "predictive_accuracy") %>% 
  group_by(data.id) %>% 
  sample_n(1) %>% 
  ungroup() %>% 
  select(task.id)

ids = ids$task.id
save(ids, file = "omlTaskIds.RData")
