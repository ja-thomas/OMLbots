exponentialBackOff = function(jobs, registry, start.resources = NULL, max.resources) {
  
  if (is.null(start.resources))
    start.resources = registry$default.resources
  
  resourceTable = data.table(job.id = jobs, walltime = start.resources$walltime, memory = start.resources$memory)
  
  while (nrow(resourceTable) > 0) {
    print(resourceTable)
    
    for (i in seq_len(nrow(resourceTable)))
      submitJobs(resourceTable[i, job.id], resources = list(walltime = resourceTable[i, walltime],
        memory = resourceTable[i, memory], ntasks = start.resources$ntasks))
    waitForJobs() 
    memory.exceeded = grepLogs(pattern = "slurmstepd: Exceeded step memory limit at some point.")$job.id
    time.exceeded = grepLogs(pattern = "slumstepd: [\\*]{3} JOB .+ DUE TO TIME LIMIT [\\*]{3}")$job.id
    
    # Drop successful jobs and increase walltime and memory for expired jobs
    resourceTable[job.id %in% memory.exceeded | (job.id %in% time.exceed & walltime < max.resources$walltime)]
    resourceTable[job.id %in% time.exceeded | (job.id %in% memory.exceed & memory < max.resources$memory)]
    resourceTable[job.id %in% memory.exceeded, memory := min(max.resources$memory, 2 * memory)]
    resourceTable[job.id %in% time.exceeded, walltime := min(max.resources$walltime, 2 * walltime)]
    
  }
}
