library(devtools)
load_all()

runBot(batch.size = 1, lrn.ps.sets = lrn.ps.sets, upload = FALSE)

system.time(overview <- getMlrRandomBotOverview("mlrRandomBotV1"))
print(overview)

results = getMlrRandomBotResults(tag = "mlrRandomBotV1")
print(results)

system.time(hypPars <- getMlrRandomBotHyperpars(tag = "mlrRandomBotV1"))
print(hypPars)

metaFeatures = getMetaFeatures(tag = "study_14")
print(head(metaFeatures))
