library(devtools)
load_all()

runBot(batch.size = 1, lrn.ps.sets = lrn.ps.sets, upload = FALSE)

system.time(overview <- getMlrRandomBotOverview("mlrRandomBotV1"))
overview

results = getMlrRandomBotResults(tag = "mlrRandomBotV1")
results

hypPars = getMlrRandomBotHyperpars(tag = "mlrRandomBotV1")
hypPars