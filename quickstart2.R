library(devtools)
load_all()
devtools::load_all("../batchtools")

# On LRZ
# Serial cluster
saveOMLConfig(apikey = "34ebc7dff3057d8e224e5beac53aea0e")
max.resources = list(walltime = 3600*5, memory = 2000)
lrn.par.set = getMultipleLearners()
simple.lrn.par.set = getSimpleLearners()
i = sample(1:1e7, 1)

runBot(1000, path = paste0("test", i), 
      sample.learner.fun = sampleRandomLearner, sample.task.fun = sampleRandomTask, 
      sample.configuration.fun = sampleRandomConfiguration,   
      lrn.ps.sets = simple.lrn.par.set, upload = TRUE, extra.tag = "botV1")
