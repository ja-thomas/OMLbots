library(devtools)
load_all()

runBot(1,path = "test")

runBot(1, sample.configuration.fun = sampleDefaultConfiguration, upload = TRUE)

overview = getMlrRandomBotOverview()
print(overview)

results = getMlrRandomBotResults()
print(results)

hypPars = getMlrRandomBotHyperpars()
print(hypPars)

metaFeatures = getMetaFeatures(tag = "study_14")
print(head(metaFeatures))
