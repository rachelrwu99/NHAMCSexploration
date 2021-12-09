# Clean NHAMCS files

library(SAScii)
library(lattice)
library(stringr)
library(foreign)
library(dplyr)
library(ggplot2)
library(survey)
library(tidyr)
library(reshape)


# Directories
#projdir <- "~/Documents/Github/NHAMCSexploration/code"
#datadir<- "~/Desktop/Friedman/AgingAbdomen/data/dta/Analysis"
projdir = "~/Documents/University/Academic Plan/Fall 2021/STAT 571/Final Project/NHAMCSexploration/code"
datadir<- "~/Documents/University/Academic Plan/Fall 2021/STAT 571/Final Project/Analysis"


# --- Loading --- #

# Read in Stata files
#setwd(file.path(projdir, "data"))
nhamcs.list <- as.list(list.files(pattern="*.dta", path=file.path(projdir,"explore", "data"), full.names = TRUE))
nhamcs.data <- lapply(nhamcs.list, read.dta)
nhamcs <- merge_all(nhamcs.data)
colnames( nhamcs ) <- tolower(colnames(nhamcs))

