.libPaths("./Rlib")
devtools::load_all()

# On LRZ
# Serial cluster
setOMLConfig(apikey = "34ebc7dff3057d8e224e5beac53aea0e")
#max.resources = list(walltime = 3600*5, memory = 2000)
#lrn.par.set = getMultipleLearners()
simple.lrn.par.set = getSimpleLearners()
i = sample(1:1e7, 1)

runBot(10, path = paste0("test", i), 
      sample.learner.fun = sampleRandomLearner, sample.task.fun = sampleSimpleTask, 
      sample.configuration.fun = sampleRandomConfiguration,   
      lrn.ps.sets = simple.lrn.par.set, upload = TRUE, extra.tag = "botV1")
