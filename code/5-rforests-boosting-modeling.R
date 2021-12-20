########### Random Forests ########
subset_admit_train <- admit_train %>%
  select( -DIAG1R, -DIAG2R, -LOV, -RFV1, -MED1, - BPDIAS, -BPSYS, -CONTSUB2,
          -RFV13D, -RFV2, -RFV23D, -RFV3, -RFV33D, -TEMPF, -WAITTIME, -RESPR, -PULSE, -POPCT)
rf_fit = randomForest(factor(ADMITHOS) ~ ., 
                      data =subset_admit_train)
plot(rf_fit)
rf_fit$err.rate %>% head()

save(rf_fit, file = "/Users/rachelwu/Documents/GitHub/NHAMCSexploration/results/rand_forest_fit.Rda")
# create error graph
png(width = 6, 
    height = 4,
    res = 300,
    units = "in", 
    filename = "/Users/rachelwu/Documents/GitHub/NHAMCSexploration/results/rand_forest_error.png")
tibble(oob_error = rf_fit$err.rate[,"OOB"],
       trees = 1:500) %>%
  ggplot(aes(x = trees, y = oob_error)) + geom_line() + theme_bw()
dev.off()

# Varying mtry
rf_3 = randomForest(factor(ADMITHOS) ~ ., mtry = 3, data = subset_admit_train)
rf_7 = randomForest(factor(ADMITHOS) ~ ., mtry = 7, data = subset_admit_train)
rf_13 = randomForest(factor(ADMITHOS) ~ ., mtry = 13, data = subset_admit_train)
oob_errors = bind_rows(
  tibble(ntree = 1:500, oob_err = rf_3$err.rate[,"OOB"], m = 3),
  tibble(ntree = 1:500, oob_err = rf_7$err.rate[,"OOB"], m = 7),
  tibble(ntree = 1:500, oob_err = rf_13$err.rate[,"OOB"], m = 13)
)

# create plot
png(width = 6, 
    height = 4,
    res = 300,
    units = "in", 
    filename = 
      "/Users/rachelwu/Documents/GitHub/NHAMCSexploration/results/rand_forest_tuning.png")
oob_errors %>%
  ggplot(aes(x = ntree, y = oob_err, colour = factor(m))) +
  geom_line() + theme_bw()
dev.off()


# Variable importance plot
rf_fitVarImp = randomForest(factor(ADMITHOS) ~ AGE + AMBTRANSFER + 
                              ANYIMAGE + ARREMS + ATTPHYS + 
                          BMP + BNP + BOARDHOS + BPAP + BPDIAS + BPSYS + 
                          CAD + CBC + CHF + CKD + CMP + COPD +
                          CPR + CTAB + CTCHEST + CTHEAD + CTOTHER +
                          CTUNK + DDIMER + CARDMON + CBC + CONSULT + 
                          CATRIAGE + DIABTYP1 + DIABTYP2 + DIABTYP0 + 
                          NUMMED + NUMGIV + PAINSCALE + PAYMCARE + PAYTYPER + POPCT + 
                          PROC +  PTTINR + PULSE + RACER + RACERETH + RACEUN + 
                          REGION + RESIDNCE + NURSEPR + RESPR + 
                          SEEN72 + RNLPN + TOTCHRON + TOTDIAG, 
                          importance = TRUE, data = admit_train)

png(width = 9, 
    height = 9,
    res = 300,
    units = "in", 
    filename = 
      "/Users/rachelwu/Documents/GitHub/NHAMCSexploration/results/rand_forest_varImpPlot.png")
varImpPlot(rf_fitVarImp, main = "Random Forest: Variable Importance Plot")
dev.off()

# Making predictions
rf_predictions = predict(rf_7, #n.trees = optimal_num_trees,
                         type = "response", newdata = admit_test)

misclas_rforest <- mean(rf_predictions != admit_test$ADMITHOS)
misclas_rforestHC <- 0.0846473


############# Boosting ###########

set.seed(1)
# Setting binary response variable to 0 - 1, NOT as a factor
admit_train$ADMITHOS <- as.numeric(as.character(admit_train$ADMITHOS))
gbm_fit = gbm(ADMITHOS ~ . - DIAG1R - DIAG2R - LOV - RFV1 - MED1,
              distribution = "bernoulli",
              n.trees = 100,
              interaction.depth = 1,
              shrinkage = 0.1,
              cv.folds = 5,
              data = admit_train)

gbm.perf(gbm_fit) #60

# try a few values
set.seed(1)
ntrees = 100
gbm_fit_1 = gbm(ADMITHOS ~ . - DIAG1R - DIAG2R - LOV - RFV1 - MED1,
                distribution = "bernoulli",
                n.trees = ntrees,
                interaction.depth = 1,
                shrinkage = 0.1,
                cv.folds = 5,
                data = admit_train)
set.seed(1)
gbm_fit_2 = gbm(ADMITHOS ~ .  - DIAG1R - DIAG2R - LOV - RFV1 - MED1,
                distribution = "bernoulli",
                n.trees = ntrees,
                interaction.depth = 2,
                shrinkage = 0.1,
                cv.folds = 5,
                data = admit_train)
set.seed(1)
gbm_fit_3 = gbm(ADMITHOS ~ .  - DIAG1R - DIAG2R - LOV - RFV1 - MED1,
                distribution = "bernoulli",
                n.trees = ntrees,
                interaction.depth = 3,
                shrinkage = 0.1,
                cv.folds = 5,
                data = admit_train)
# extract CV errors

cv_errors = bind_rows(tibble(ntree = 1:ntrees, cv_err = gbm_fit_1$cv.error, depth = 1),
                      tibble(ntree = 1:ntrees, cv_err = gbm_fit_2$cv.error, depth = 2),
                      tibble(ntree = 1:ntrees, cv_err = gbm_fit_3$cv.error, depth = 3)
)
# plot CV errors
png(width = 6, 
    height = 4,
    res = 300,
    units = "in", 
    filename = "/Users/rachelwu/Documents/GitHub/NHAMCSexploration/results/boosting_cverror_plot.png")
cv_errors %>%
  ggplot(aes(x = ntree, y = cv_err, colour = factor(depth))) +
  geom_line() + theme_bw()
dev.off()


# partial dependence plots
gbm_fit_optimal = gbm_fit_2
optimal_num_trees = gbm.perf(gbm_fit_2, plot.it = FALSE)
var_sum <- summary(gbm_fit_optimal, n.trees = optimal_num_trees, plotit = FALSE) %>%
  head(12) 
var_sum %>% write_tsv("/Users/rachelwu/Documents/GitHub/NHAMCSexploration/results/boosting_var_summary.tsv")#want this table as a kable 

png(width = 6, 
    height = 4,
    res = 300,
    units = "in", 
    filename = 
      "/Users/rachelwu/Documents/GitHub/NHAMCSexploration/results/boosting_age_partial_dependence_plot.png")
plot(gbm_fit_optimal, i.var = "AGE", n.trees = optimal_num_trees, type = "response")
dev.off()

png(width = 6, 
    height = 4,
    res = 300,
    units = "in", 
    filename = 
      "/Users/rachelwu/Documents/GitHub/NHAMCSexploration/results/boosting_totdiag_partial_dependence_plot.png")
plot(gbm_fit_optimal, i.var = "TOTDIAG", n.trees = optimal_num_trees, type = "response")
dev.off()

# Making predictions 

gbm_probabilities = predict(gbm_fit_optimal, n.trees = optimal_num_trees,
                            type = "response", newdata = admit_test)
gbm_predictions = as.numeric(gbm_probabilities > 0.5)
misclas_boost <- mean(gbm_predictions != admit_test$ADMITHOS)
misclas_boostHC <- 0.09875519
