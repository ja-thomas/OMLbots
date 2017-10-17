library(devtools)
load_all()
lrn.par.set = getMultipleLearners()

load("./snapshot_database/mlrRandomBotResultsTables.RData")

head(results[[2]])

pdf("Distribution_of_Runs", width = 15, height = 10)
# How many runs per learner?
barplot(sapply(results, nrow))

# How many runs per task?
barplot(table(results[[1]]$task_id), xlab = "task_id", ylab = "number of runs", main = names(results)[1])
# one task with much more runs
barplot(table(results[[2]]$task_id), xlab = "task_id", ylab = "number of runs", main = names(results)[2])
barplot(table(results[[3]]$task_id), xlab = "task_id", ylab = "number of runs", main = names(results)[3])
barplot(table(results[[4]]$task_id), xlab = "task_id", ylab = "number of runs", main = names(results)[4])
barplot(table(results[[5]]$task_id), xlab = "task_id", ylab = "number of runs", main = names(results)[5])
barplot(table(results[[6]]$task_id), xlab = "task_id", ylab = "number of runs", main = names(results)[6])
dev.off()

# Distribution of the hyperparameters
pdf("Distribution_of_Hyperparameters", width = 15, height = 10)
for(j in 1:length(results)) {
  print(j)
  nr.pars = which(names(results[[j]]) == "area.under.roc.curve") - 2
  pars = names(results[[j]])[2:c(1 + nr.pars)]
  
  for(i in seq_along(pars)) {
    print(paste(j, i))
    type = lrn.par.set[[j]]$param.set$pars[[pars[i]]]$type
    if(type %in% c("numeric", "numericvector", "integer"))
      hist(as.numeric(results[[j]][, pars[i]]), xlab = pars[i], ylab = "number of runs", main = names(results)[j])
    else
      barplot(table(results[[j]][, pars[i]]), xlab = pars[i], ylab = "number of runs", main = names(results)[j])
  }
}

# per dataset
for(j in seq_along(results)) {
  print(j)
  nr.pars = which(names(results[[j]]) == "area.under.roc.curve") - 2
  pars = names(results[[j]])[2:c(1 + nr.pars)]
  for(i in seq_along(pars)) {
    par(mfrow = c(7, 10), oma=c(5,4,2,0) + 0.1,
      mar = c(1,1,3,1) + 0.1)
    
    paste0(names(results)[j], pars[i])
    
    task_ids = results[[j]]$task_id
    uni_tasks = sort(unique(task_ids))
    for(h in seq_along(uni_tasks)) {
      print(paste(j, i, h))
      type = lrn.par.set[[j]]$param.set$pars[[pars[i]]]$type
      error = NULL
      if(type %in% c("numeric", "numericvector", "integer"))
        error = try(hist(as.numeric(results[[j]][task_ids == uni_tasks[h], pars[i]]), xlab = "", ylab = "", main = paste("task_id" ,uni_tasks[h])))
      else
        barplot(table(results[[j]][, pars[i]]), xlab = pars[i], ylab = "", main = paste("tskid" ,uni_tasks[h]))
      if(class(error) == "try-error"){
        plot.new()
        text(x = 0.5, y = 0.5, paste("id" ,uni_tasks[h]), cex = 1, col = "black")
      }
    }
    title(paste(names(results)[j], pars[i]), outer=TRUE)
  }
}
dev.off()
# put it into a shiny app