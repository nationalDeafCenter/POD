library(tidyverse)
library(mice)
library(lme4)
library(lmerTest)
library(texreg)
select <- dplyr::select

### goal: do age, race, gender, accomodations, additional disabilities, or institution type (4-year or community college) predict survey responses?

## step 0 (in Excel): save "data/20190723 POD.xlsx" as "data/POD1-9-19.csv" and also make "data/varDesc725.csv" which classifies each of the survey enteries

## step 1: general data cleaning, make variables suitable for modeling, etc
source('code/cleanData.r')

## setp 2: make data for regressions--put all regressors in final form, multiply-impute missing values
source('code/regDatPrep.r') ## warning this takes a long time to run
## makes file "data/dataForRegressions.r" including 20 multiple imuputed datasets (impDat)


## step 3: load functions for regressions
## specifically:
## transMIdat() which takes multiple imputed wide data and makes multiple imputed long data
##     in long data, each subject shows up 6 times, once for each of the scales
## fullTrtObj() which
### 1) pools models fit to multiple imputed datasets and
### 2) makes a texreg object that can be put into a regression table
source('code/regFunctions.r')

#step 4: run the actual models, make the regression table.
source('code/simpleRegressions.r')
