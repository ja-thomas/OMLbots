library(devtools)
load_all()

runBot(batch.size = 5, lrn.ps.sets = lrn.ps.sets, upload = TRUE)
