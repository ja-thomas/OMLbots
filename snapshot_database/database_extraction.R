############################################################################
library(RMySQL)
library(dplyr)

mydb = dbConnect(MySQL(), user = "root", dbname='openml')
dbListTables(mydb)

############################# evaluation_results ###########################

# evaluation table
evaluation =  dbSendQuery(mydb, "select * from evaluation")
evaluation = fetch(evaluation, n=-1)
head(evaluation)
# only AUC and runtime
# id of math_function identifies the measure in evaluation (function_id)
evaluation = evaluation[evaluation$function_id %in% c(4, 63),]
# 6301439 evaluations
colnames(evaluation)[1] = "rid"
evaluation = evaluation[, c("rid", "function_id", "value")]

# run table
run =  dbSendQuery(mydb, "select * from run")
run = fetch(run, n=-1)
head(run)
table(unique(evaluation$source) %in% unique(run$rid))
# ok!
run = run[, c("rid", "uploader", "setup", "task_id")]

# Join tables to evaluation_results
evaluation_results = inner_join(run, evaluation, by = "rid")
head(evaluation_results)
# only runs of the open_ml bot
evaluation_results = evaluation_results[evaluation_results$uploader == 2702,]
head(evaluation_results)

############################# meta_features ################################

# data quality table
data_quality =  dbSendQuery(mydb, "select * from data_quality")
data_quality = fetch(data_quality, n=-1)
head(data_quality)
data_quality = data_quality[, c("data", "quality", "value")]
table(data_quality$data)

# input data table
input_data =  dbSendQuery(mydb, "select * from input_data")
input_data = fetch(input_data, n=-1)
head(input_data)
input_data = input_data[, c("run", "data")]
colnames(input_data)[1] = "rid"

# Add the data id to the evaluation results as identifier
evaluation_results = inner_join(evaluation_results, input_data, "rid")
head(evaluation_results)

# Only keep relevant informations of relevant datasets
meta_features_names = c("MajorityClassSize", "MajorityClassPercentage", "NumberOfClasses", 
  "NumberOfInstances", "NumberOfFeatures", "NumberOfNumericFeatures", "NumberOfSymbolicFeatures")
meta_features = data_quality[data_quality$quality %in% meta_features_names, ]
meta_features = meta_features[meta_features$data %in% unique(evaluation_results$data), ]
head(meta_features)

############################# hyperparameters ################################

input_setting = dbSendQuery(mydb, "select * from input_setting")
input_setting = fetch(input_setting, n=-1)
head(input_setting)
input_setting = input_setting[input_setting$setup %in% unique(evaluation_results$setup),]

input =  dbSendQuery(mydb, "select * from input")
input = fetch(input, n=-1)
head(input)
colnames(input)[1] = "input_id"
input = input[, c("input_id", "fullName", "name")]

hyperparameters = inner_join(input_setting, input, by = "input_id")
head(hyperparameters)
hyperparameters = hyperparameters[!(hyperparameters$name %in% c("nthread", "num.threads", "openml.kind", "openml.normal.kind", "openml.seed", "s", "verbose", "xval")), ]
hyperparameters$fullName = gsub("\\(.*","",hyperparameters$fullName)
table(hyperparameters$name)


# Add default values


