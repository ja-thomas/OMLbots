#' Sample a random learner with matching parameter set from the lrn.ps.sets list
#' @param lrn.ps.sets of available learners with matching parameter sets
#' @return list of one learner with matching parameter set
#' @export
sampleRandomLearner = function(lrn.ps.sets) {
  sample(lrn.ps.sets, size = 1)[[1]]
}