library(OpenML)
library(mlr)

# Put the results into a table
# get performances
time = system.time(my_runs <- listOMLRunEvaluations(tag = "botV1", evaluation.measure = "area_under_roc_curve", limit = NULL))
# subset results on some information(s) and measure(s)
my_runs = my_runs[, c("run.id", "task.id", "area.under.roc.curve" )]

# get hyperparameters
runs = listOMLRuns(tag = "botV1")
paras = listOMLSetup(runs$setup.id)
paras_names = names(lrn.par.set$pars)
paras = paras[paras$parameter.name %in% paras_names, c("setup.id", "parameter.name", "value")]
library(tidyr)
library(dplyr)
paras = spread(paras, key = parameter.name, value = value)
paras = merge(paras, runs[, c("run.id", "setup.id")], key = "setup.id")
paras = select(paras, -setup.id)

# Put things together
results = merge(my_runs, paras, by = "run.id")
# Put it in a nice order
results = results[, c(setdiff(names(results), "area.under.roc.curve"), "area.under.roc.curve")]
# Now you can compare the performances of your different hyperparameters
print(head(results))

