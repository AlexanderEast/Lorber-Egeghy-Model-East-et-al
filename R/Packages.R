# Required Packages for EPA's Lorber-Egeghy Model
# AE, ORAU, 2020.

requiredpackages<-  c("rio",
                      "plyr",
                      "dplyr",
                      "readxl",
                      "stringr",
                      "tidyverse",
                      "data.table",)

neededpackages<-setdiff(requiredpackages,rownames(installed.packages()))

install.packages(neededpackages)
library('rio')
library('plyr')
library('dplyr')
library('readxl')
library('stringr')
library('tidyverse')
library('data.table')
rm(requiredpackages,neededpackages)
