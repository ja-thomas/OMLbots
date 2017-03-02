library(devtools)
load_all()

runBot(1)

runBot(1, sample.configuration.fun = sampleDefaultConfiguration, upload = TRUE)

system.time(overview <- getMlrRandomBotOverview("mlrRandomBotV1"))
print(overview)

results = getMlrRandomBotResults(tag = "mlrRandomBotV1")
print(results)

system.time(hypPars <- getMlrRandomBotHyperpars(tag = "mlrRandomBotV1"))
print(hypPars)

metaFeatures = getMetaFeatures(tag = "study_14")
print(head(metaFeatures))
