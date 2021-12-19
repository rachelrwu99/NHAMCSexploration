# Load NHAMCS files
library(keras)         # to train neural networks
library(kableExtra)    # to print tables
library(cowplot)       # to print side-by-side plots
library(tidyverse)     # tidyverse
library(tensorflow)
library(dplyr)
library(haven)
library(pROC) # for ROC curves
library(ggplot2)
library(haven)
library(corrplot)
library(ggcorrplot)
library(glmnetUtils)
library(rpart) # install.packages("rpart")
library(rpart.plot) # install.packages("rpart.plot")
library(randomForest) # install.packages("randomForest")
library(gbm) # install.packages("gbm")



url_2019 = "https://ftp.cdc.gov/pub/Health_Statistics/NCHS/dataset_documentation/nhamcs/stata/ED2019-stata.zip"
url_2018 = "https://ftp.cdc.gov/pub/Health_Statistics/NCHS/dataset_documentation/nhamcs/stata/ED2018-stata.zip"
url_2017 = "https://ftp.cdc.gov/pub/Health_Statistics/NCHS/dataset_documentation/nhamcs/stata/ed2017-stata.zip"
url_2016 = "https://ftp.cdc.gov/pub/Health_Statistics/NCHS/dataset_documentation/nhamcs/stata/ED2016-stata.zip"
url_2015 = "https://ftp.cdc.gov/pub/Health_Statistics/NCHS/dataset_documentation/nhamcs/stata/ED2015-stata.zip"
temp = tempfile()
download.file(url_2019,temp)
data_2019 = read_dta(unz(temp,"ED2019-stata.dta"))

temp = tempfile()
download.file(url_2018,temp)
data_2018 = read_dta(unz(temp,"ED2018-stata.dta"))

temp = tempfile()
download.file(url_2017,temp)
data_2017 = read_dta(unz(temp,"ed2017-stata.dta"))

temp = tempfile()
download.file(url_2016,temp)
data_2016 = read_dta(unz(temp,"ED2016-stata.dta"))

temp = tempfile()
download.file(url_2015,temp)
data_2015 = read_dta(unz(temp,"ED2015-stata.dta"))

data_raw = bind_rows(data_2015, data_2016, data_2017, data_2018, data_2019)
