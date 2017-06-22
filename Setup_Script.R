# RUN: sudoedit /usr/lib64/microsoft-r/3.3/lib64/R/etc/Makeconf
# EDIT: Cxx1x = gcc
# CXX1XFLAGS = -fpic
# CXX1XPICFLAGS =
# CXX1XSTD = -std=c++11

#.libPaths("./Rlib")
#install.packages("batchtools", repos = "https://cloud.r-project.org")
#install.packages("OpenML", repos = "https://cloud.r-project.org")
#install.packages("tidyr", repos = "https://cloud.r-project.org")
#install.packages("rscimark", repos = "https://cloud.r-project.org")
#install.packages("stringi", repos = "https://cloud.r-project.org")
#install.packages("devtools", repos = "https://cloud.r-project.org")
#install.packages("roxygen2", repos = "https://cloud.r-project.org")
#install.packages("RCurl", repos = "https://cloud.r-project.org")
#install.packages("emoa", repos = "https://cloud.r-project.org")
#install.packages("farff", repos = "https://cloud.r-project.org")
#install.packages("snow", repos = "https://cloud.r-project.org")
#install.packages("plotly", repos = "https://cloud.r-project.org")
#install.packages("glmnet", repos = "https://cloud.r-project.org")
#install.packages("kknn", repos = "https://cloud.r-project.org")
#install.packages("e1071", repos = "https://cloud.r-project.org")
#install.packages("dummies", repos = "https://cloud.r-project.org")
#install.packages("ggplot2", repos = "https://cloud.r-project.org")
#install.packages("plotly", repos = "https://cloud.r-project.org")
#install.packages("ranger", repos = "https://cloud.r-project.org") #FIXME
#install.packages("xgboost", repos = "https://cloud.r-project.org") #FIXME

system("python -m pip install --user benchexec")
file.copy("./linux-helper", "./Rlib/batchtools/bin/linux-helper", overwrite = TRUE)
