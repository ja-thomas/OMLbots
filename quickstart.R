library(devtools)
load_all()

# On LRZ
# Serial cluster
setOMLConfig(apikey = "34ebc7dff3057d8e224e5beac53aea0e")
max.resources = list(walltime = 3600*5, memory = 2000)
lrn.par.set = getMultipleLearners()
simple.lrn.par.set = getSimpleLearners()

for(i in 1:10000) {
  try(runBot(30, path = paste0("/naslx/projects/ua341/di49ruw/test", i), 
    sample.learner.fun = sampleRandomLearner, sample.task.fun = sampleRandomTask, 
    sample.configuration.fun = sampleRandomConfiguration, max.resources = max.resources,  
    lrn.ps.sets = lrn.par.set, upload = TRUE, extra.tag = "botV1"))
}


missing_tasks = c(272, 282, 3896, 3917, 7295, 9889, 9910, 9911, 9957, 9976, 34539, 145834, 145836, 145847, 145848,
  145853, 145854, 145857, 145862, 145872, 145878, 145972, 145976, 145979, 146012, 146064, 146066, 146082, 146085)

sampleRandomkknnTask = function(x) {
  tasks = listOMLTasks(number.of.classes = 2L, number.of.missing.values = 0, 
    data.tag = "study_14", estimation.procedure = "10-fold Crossvalidation")
  tasks = tasks[tasks$task.id %in% missing_tasks,]
  messagef("Found %i available OML tasks", nrow(tasks))
  task = tasks %>% 
    filter(format == "ARFF", status == "active") %>% 
    sample_n(1) %>% 
    select(task.id, name)
  
  return(list(id = task$task.id, name = task$name))
}

for(i in 1:10000) {
  try(runBot(30, path = paste0("/naslx/projects/ua341/di49ruw/test", i), 
    sample.learner.fun = sampleRandomLearner, sample.task.fun = sampleRandomkknnTask, 
    sample.configuration.fun = sampleRandomConfiguration, max.resources = max.resources,  
    lrn.ps.sets = lrn.par.set, upload = TRUE, extra.tag = "botV1"))
}



# Locally
for(i in 1:1000){
  try(runBot(100, path = paste0("test", i), 
    sample.learner.fun = sampleRandomLearner, sample.task.fun = sampleRandomTask, 
    sample.configuration.fun = sampleRandomConfiguration,   
    lrn.ps.sets = lrn.par.set, upload = TRUE, extra.tag = "botV1"))
}

# Geht nicht bei zu großen Ergebnissen; stattdessen mit limit und listOMLRuns 
tag = "mlrRandomBot"
numRuns = 160000
results = do.call("rbind", 
  lapply(0:floor(numRuns/10000), function(i) {
    return(listOMLRuns(tag = tag, limit = 10000, offset = (10000 * i) + 1))
  })
)
table(results$flow.id, results$task.id)
table(results$uploader)

?listOMLSetup

