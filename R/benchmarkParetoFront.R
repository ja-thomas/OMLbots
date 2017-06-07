benchmarkParetoFront = function(measure.name = "area.under.roc.curve", 
  learner.name, task.ids, lrn.par.set, tbl.results, tbl.hypPars, 
  tbl.metaFeatures, tbl.runTime, tbl.resultsReference, surrogate.mlr.lrn, min.experiments = 100) {
  
  # Leave one (or several) dataset(s) out
  datasets = unique(tbl.results$task.id[tbl.results])
  for(i in seq_along(datasets))
    excl = datasets[i]
    tbl.results.i = tbl.results[tbl.results$task.id != excl, ]
  # Create surrogate model
  
  surrogate.measures = makeSurrogateModel(measure.name = "area.under.roc.curve", 
    learner.name = learner.name, task.ids, lrn.par.set, tbl.results.i, tbl.hypPars, 
    tbl.metaFeatures, tbl.runTime, tbl.resultsReference, surrogate.mlr.lrn, min.experiments = 100)
  surrogate.time = makeSurrogateModel(measure.name = "area.under.roc.curve", 
    learner.name = learner.names[i], task.ids, lrn.par.set, tbl.results, tbl.hypPars, 
    tbl.metaFeatures, tbl.runTime, tbl.resultsReference, surrogate.mlr.lrn, min.experiments = 100, 
    time = TRUE)
  
  # Create pareto front for the left out datasets
  # Compare this pareto front with
    # the real pareto front (created by a local surrogate model?) (visualization? with moving points...)
    # the default value
      # is it dominated by the pareto front?
      # Is the best predicted performance point better?
  
}