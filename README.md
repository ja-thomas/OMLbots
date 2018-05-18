# OMLbots

Bot that executes (random) experiments on OpenML datasets and uploads the results on the OpenML platform. 

The main function of the bot can be executed via `runBot`. 

See executed runs on the openml.org page: https://www.openml.org/u/2702

## OpenML Identification

Name: **OpenML_Bot R**

ID: 2702

## Tags

General Tag: **mlrRandomBot**

Extra-Tag for the random hyperparameter runs (without RF default runs): **botV1**

Extra-Tag for the reference runs (random forest with defaults): **referenceV1**

## Downloading

### The fixed subset of 2.5 million results

A fixed subset of the results of the random bot can be downloaded easily from figshare: 

https://figshare.com/articles/OpenML_R_Bot_Benchmark_Data_final_subset_/5882230

This dataset is described soon in a paper. 

### All results via the nightly database snapshot

Alternatively all results can be downloaded via the nightly database snapshot.
The snapshot can be downloaded from: https://docs.openml.org/developers/

After having set up the SQL database (see [here](https://github.com/ja-thomas/OMLbots/blob/master/snapshot_database/readme_download) for an example how to do it via a terminal in linux), the data can be extracted with this code: https://github.com/ja-thomas/OMLbots/blob/master/snapshot_database/database_extraction.R

### Using the R-API

If you want to download results via the OpenML package you can use the following code. (Currently under review, does not work yet.)

https://github.com/ja-thomas/OMLbots/blob/master/GetResultsR-API.R
