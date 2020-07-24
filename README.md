# pacific-cod-2020
Draft update to the Pacific Cod assessment for Areas 3CD and 5ABCD. 

This is a simple "turn of the crank" on the base case models with updated data streams.

## Getting started

**Create the following folders:**

* models

* data/generated

* data/pcod-cache

* data/results

* presentations

* presentations/figures (can get rid of this one later when I look for the code that uses it)

Some of these folders might make themselves but I can't remember which ones.

## Model files

* Put all the model files in the models folder



### Make the data objects

You can get on the VPN and just knit the whole document, but maybe best to make the data objects first to avoid confounding issues and lengthy waiting.

To make the data objects first, get on the VPN then:

1. Run the chunks in 00-load.Rmd. This will: make the main data file ("data/pcod-cache/pacific-cod.rds", made in in "R/all.R""); and make the catch objects (in "R/custom-knitr-variables.R")

2. Run the chunks in "report/05-cpue-app.Rmd" to run the CPUE analyses and make the data objects. This will take a while the first time.

3. Source "data/get-mean-weight.R". Do this after you have run all.R (Step 1) to avoid having to pull the data again.

If problems with any of the queries, ask Robyn for the RDS files. You will need:

1. Put these files directly into the data folder:

* cpue-historic.rds

* cpue-modern.rds

2. Put this file into the data/pcod-cache folder:

* pacific-cod.rds

3. Put these files into the data/generated folder

* all_surveys.csv

* cpue-predictions-historical.csv

* cpue-predictions-modern.csv

* cpue-re-preds.rda

* cpue-re-preds-noint.rda

* cpue-models-pcod-historic.rds

* cpue-models-pcod-modern.rds





