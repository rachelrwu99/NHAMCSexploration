train_samples = sample(1:nrow(data_factored), 0.8*nrow(data_factored))
admit_train = data_factored %>% filter(row_number() %in% train_samples)
admit_test = data_factored %>% filter(!(row_number() %in% train_samples))

lasso_fit50 = cv.glmnet(ADMITHOS ~ ., # formula notation, as usual
                      alpha = 1, # alpha = 1 for lasso
                      nfolds = 10, # number of folds
                      family = "binomial", # to specify logistic regression
                      type.measure = "class", # use misclassification error in CV 
                      data = admit_train) # data to run lasso on

# save the lasso fit object
save(lasso_fit50, file = "results/lasso_fit50.Rda")

# create lasso CV plot
png(width = 6, 
    height = 4,
    res = 300,
    units = "in", 
    filename = "/Users/rachelwu/Documents/GitHub/NHAMCSexploration/results/lasso-cv-plot.png")
plot(lasso_fit50)
dev.off()

# create lasso trace plot
p = plot_glmnet(lasso_fit50, admit_train, features_to_plot = 10)
ggsave(filename = "/Users/rachelwu/Documents/GitHub/NHAMCSexploration/results/lasso-trace-plot.png", 
       plot = p, 
       device = "png", 
       width = 6, 
       height = 4)


# Making predictions
lasso50_predictions = predict(lasso_fit50, 
                            newdata = admit_test,
                            s = "lambda.1se") %>% as.numeric()
lasso50_predictions = as.numeric(lasso50_predictions > 0.5)

admit_test_pred50 = admit_test %>%
  mutate(predicted_admit_lasso = lasso50_predictions)

# then calculate misclassification rate
admit_test_pred50 %>%
  summarise(mean(ADMITHOS != predicted_admit_lasso))
