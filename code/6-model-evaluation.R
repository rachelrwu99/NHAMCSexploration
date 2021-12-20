# load libraries
library(glmnetUtils)
library(tidyverse)

misclass_linear_reg

misclas_lassoHC

misclas_ridgeHC

misclas_elasticHC

misclas_tree_unprunedHC

misclas_tree_prunedHC

misclas_rforestHC

misclas_boostHC

# print nice table
tibble(Model = c("Logistic Regression", "Lasso", "Ridge", "Elastic", 
                 "Unpruned Tree", "Pruned Tree", "Random Forest", "Boosting"), 
       `Misclassification Error` = c(misclass_linear_reg, misclas_lassoHC, 
                                     misclas_ridgeHC, misclas_elasticHC, 
                                     misclas_tree_unprunedHC, 
                                     misclas_tree_prunedHC, misclas_rforestHC, 
                                     misclas_boostHC)) %>%
  write_tsv("/Users/leo/Documents/University/Academic Plan/Fall 2021/STAT 571/Final Project/NHAMCSexploration/results/model-evaluation.tsv")