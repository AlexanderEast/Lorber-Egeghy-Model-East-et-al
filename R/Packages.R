# Required Packages for EPA's Lorber-Egeghy Model
# AE, ORAU, 2020.

requiredpackages<-  c("rio",
                      "dplyr",
                      "readxl",
                      "stringr",
                      "tidyverse",
                      "data.table",
                      "plyr")

neededpackages<-setdiff(requiredpackages,rownames(installed.packages()))

install.packages(neededpackages)

library('dplyr')
library('readxl')
library('stringr')
library('tidyverse')
library('data.table')
library('plyr')
library('rio')
rm(requiredpackages,neededpackages)
