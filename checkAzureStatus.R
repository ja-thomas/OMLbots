library(parallel)
library(OpenML)
library(data.table)
setOMLConfig(apikey = "34ebc7dff3057d8e224e5beac53aea0e")

listFullOMLRuns = function(tag = NULL){
  max.limit = 10000 #FIXME: fix this once OpenML offers new solution
  
  callOMLRuns = function(i) {
    run.df = listOMLRuns(tag = tag, 
                         limit = max.limit,
                         offset = (max.limit * i) + 1)
    return(run.df)
  }
  
  df = tryCatch({listOMLRuns(tag = tag)},
                error = function(err){
                  nr.results = stringr::str_extract(err, "[0-9]{5,}")
                  message(paste0("Adjusting function to get ", nr.results, " results."))
                  
                  ls = lapply(seq(0, as.numeric(nr.results)/max.limit), function(i) try(callOMLRuns(i)))
                  results = do.call("rbind", ls)
                  return(results)
                })
  
  return(df)
}

cl <- makeCluster(getOption("cl.cores", 20))
clusterExport(cl, "listFullOMLRuns")
clusterEvalQ(cl, library(OpenML))
data = parLapply(cl, 1:25, function(x) listFullOMLRuns(tag = paste0("AzureBot", x)))

dt = do.call(rbind, data)
setDT(dt)
dt[, GetDate := as.character(Sys.Date())]
dt[, AzureBot := tstrsplit(dt$tags, ",", keep = 1L)]
dt[,.N,by=AzureBot]
dt[,.N,by=flow.id]
dt[,.N,by=task.id]

dt_agg = dt[,.(NrRuns = .N), by=.(task.id, flow.id, uploader, GetDate, AzureBot)]
fwrite(dt_agg, "Azure_Overview.csv", append = TRUE)





