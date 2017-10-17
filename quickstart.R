library(devtools)
load_all()

# On LRZ
# Serial cluster
setOMLConfig(apikey = "34ebc7dff3057d8e224e5beac53aea0e")
max.resources = list(walltime = 3600*5, memory = 2000)
lrn.par.set = getMultipleLearners()
simple.lrn.par.set = getSimpleLearners()

for(i in 1:1000) {
  try(runBot(500, path = paste0("/naslx/projects/ua341/di49ruw/test", i), 
    sample.learner.fun = sampleRandomLearner, sample.task.fun = sampleRandomTask, 
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

