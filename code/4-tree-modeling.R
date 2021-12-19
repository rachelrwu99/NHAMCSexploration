################## Unpruned decision tree ##################
split.fun <- function(x, labs, digits, varlen, faclen)
{
  # replace commas with spaces (needed for strwrap)
  labs <- gsub(",", " ", labs)
  for(i in 1:length(labs)) {
    # split labs[i] into multiple lines
    labs[i] <- paste(strwrap(labs[i], width=25), collapse="\n")
  }
  labs
}

tree_fit = rpart(ADMITHOS ~ .,
                 method = "class", # classification
                 parms = list(split = "gini"), # Gini index for splitting
                 data = admit_train)
save(tree_fit, file = "results/tree_fit.Rda")
# create plot
png(width = 6, 
    height = 4,
    res = 300,
    units = "in", 
    filename = "/Users/rachelwu/Documents/GitHub/NHAMCSexploration/results/tree_plot.png")
rpart.plot(tree_fit, split.fun=split.fun)
dev.off()

pred_tree = predict(tree_fit, newdata = admit_test, type = "class")

# confusion matrix
table(pred_tree, truth = admit_test$ADMITHOS)

# misclassification error
misclas_tree_unpruned <- mean(pred_tree != admit_test$ADMITHOS)

################### Pruned decision tree ###############
# CP Table
cp_table = printcp(tree_fit) %>% as_tibble()

cp_plot <- cp_table %>%
  ggplot(aes(x = nsplit+1, y = xerror,
             ymin = xerror - xstd, ymax = xerror + xstd)) +
  geom_point() + geom_line() +
  geom_errorbar(width = 0.2) +
  xlab("Number of terminal nodes") + ylab("CV error") +
  geom_hline(aes(yintercept = min(xerror)), linetype = "dashed") +
  theme_bw()

save(tree_pruned_fit, file = "results/tree_pruned_fit.Rda")
# create plot
png(width = 6, 
    height = 4,
    res = 300,
    units = "in", 
    filename = "/Users/rachelwu/Documents/GitHub/NHAMCSexploration/results/tree_pruned_plot.png")
cp_table %>%
  ggplot(aes(x = nsplit+1, y = xerror,
             ymin = xerror - xstd, ymax = xerror + xstd)) +
  geom_point() + geom_line() +
  geom_errorbar(width = 0.2) +
  xlab("Number of terminal nodes") + ylab("CV error") +
  geom_hline(aes(yintercept = min(xerror)), linetype = "dashed") + 
  ggtitle("Pruned Tree CV Error Plot") +
  theme_bw()
dev.off()

# Produce optimal tree
optimal_tree_info = cp_table %>%
  filter(xerror - xstd < min(xerror)) %>%
  arrange(nsplit) %>%
  head(1)
#optimal_tree_info

optimal_tree = prune(tree_fit, cp = optimal_tree_info$CP)

# create plot
png(width = 6, 
    height = 4,
    res = 300,
    units = "in", 
    filename = "/Users/rachelwu/Documents/GitHub/NHAMCSexploration/results/tree_pruned.png")
rpart.plot(optimal_tree)
dev.off()



# Predict using optimal tree
pred = predict(optimal_tree, newdata = admit_test, type = "class")
misclas_tree_pruned <- mean(pred != admit_test$ADMITHOS)
