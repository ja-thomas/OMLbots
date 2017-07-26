library(dplyr)

openml = src_mysql("openml")
openml

runs = tbl(openml, "run")
runeval = tbl(openml, "run_evaluated")
runfile = tbl(openml, "runfile")
run_tag = tbl(openml, "run_tag")

evaluation = tbl(openml, "evaluation")
glimpse(evaluation)
evaluation %>%
  group_by(evaluation_engine_id) %>%
  summarise(avg_delay = mean(value))

math_function = tbl(openml, "math_function")

head(runs)


library(RMySQL)

mydb = dbConnect(MySQL(), user = "root", dbname='openml')
dbListTables(mydb)
dbListFields(mydb, 'math_function')
run_evals = dbSendQuery(mydb, "select * from run_evaluated")
runs = fetch(run_tags, n=-1)
head(runs)
# 

evaluation =  dbSendQuery(mydb, "select * from evaluation")
evals = fetch(evaluation, n=-1)
head(evals)
# source müsste die run_id sein...

aucs = evals[evals$function_id == 4,]
head(aucs)
dim(aucs)
dim(runs)
# id of math_function identifies the measure in evaluation (function_id)

evaluation_engine =  dbSendQuery(mydb, "select * from evaluation_engine")
evaluation_engine_ = fetch(evaluation_engine, n=-1)
head(evals)

run =  dbSendQuery(mydb, "select * from run")
run_ = fetch(run, n=-1)
head(run_)
uploader = table(run_$uploader)
uploader[uploader > 100000]
run_ = run_[run_$uploader == 2702,]
# uploader id 2702 ist der OpenML Bot
table(run_$task_id)

run_ids = unique(evals$source)
mean(run_$rid %in% run_ids)
# passt nicht...

# mit data.table oder dplyr arbeiten








# Anhang
src_sqlite("/home/probst/Paper/Exploration_of_Hyperparameters/OMLbots/snapshot_database/ExpDB_SNAPSHOT.sql", create = FALSE)
src_mysql("/home/probst/Paper/Exploration_of_Hyperparameters/OMLbots/snapshot_database/ExpDB_SNAPSHOT.sql")
?src_sql


library(RODBC)
myconn <-odbcConnect("/home/probst/Paper/Exploration_of_Hyperparameters/OMLbots/snapshot_database/ExpDB_SNAPSHOT.sql", uid="", pwd="")
?odbcConnect


odbcDataSources()

library(ProjectTemplate)
sql.reader("ExpDB_SNAPSHOT.sql", "/home/probst/Paper/Exploration_of_Hyperparameters/OMLbots/snapshot_database", "openml_db")

library(dbConnect)
dbGrace=dbConnect(MySQL(),user="root",
  host="localhost",
  dbname="openml_expdb",
  password="",
  unix.sock="/home/probst/Paper/Exploration_of_Hyperparameters/OMLbots/snapshot_database/ExpDB_SNAPSHOT.sql")

odbcDataSources()


require(RMySQL)
drv <- dbDriver("MySQL")
con <- dbConnect(drv, username = "root")
dbListTables(con)
