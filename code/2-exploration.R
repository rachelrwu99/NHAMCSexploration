# load libraries
library(kableExtra)                     # for printing tables
library(cowplot)                        # for side by side plots
library(lubridate)                      # for dealing with dates
library(tidyverse)
library(ggplot2)

# create age distribution plot

data_factored %>%
  ggplot(aes(x = AGE)) +
  # create histogram for age distribution 
  geom_histogram(binwidth = 1) +
  labs(x = "Age", y = "Count") +
  # add vertical line to indicate median age 
  geom_vline(xintercept = median(data_factored$AGE), color = "red",
             linetype = "dashed") +
  theme_bw() + theme(legend.position = "none")

ggsave(filename = "/Users/leo/Documents/University/Academic Plan/Fall 2021/STAT 571/Final Project/NHAMCSexploration/results/age-distribution-plot.png", 
      plot = last_plot(), 
      device = "png", 
       width = 6, 
       height = 4)
dev.off()

# plot for response variable ADMITHOS
data_factored %>%
  ggplot(aes(x = ADMITHOS)) +
  labs(x = "Admitted to hospital", y = "Count") +
  geom_bar(stat = "count") + 
  theme_bw() + theme(legend.position = "none")

ggsave(filename = "/Users/leo/Documents/University/Academic Plan/Fall 2021/STAT 571/Final Project/NHAMCSexploration/results/admit-distribution-plot.png", 
       plot = last_plot(), 
       device = "png", 
       width = 6, 
       height = 4)
dev.off()

# plot for hospital admittance by race
race_admit = data_factored %>%
  group_by(RACER, ADMITHOS) %>%
  summarise(cnt = n()) %>%
  mutate(admit_rate = formattable::percent(cnt / sum(cnt))) %>%
  filter(ADMITHOS==1) %>%
  cbind(race = c("Non-Hispanic White","Non-Hispanic Black", "Hispanic")) %>%
  select(-c(ADMITHOS,cnt))# %>%
  
race_admit %>%
  ggplot(aes(x = race, y = admit_rate)) +
  labs(x = "Race", y="Hospital admittance rate") +
  geom_bar(stat="identity") + 
  theme_bw() + theme(legend.position = "none") +
  geom_text(aes(label = admit_rate), vjust = -0.2)

ggsave(filename = "/Users/leo/Documents/University/Academic Plan/Fall 2021/STAT 571/Final Project/NHAMCSexploration/results/race-admit-plot.png", 
       plot = last_plot(), 
       device = "png", 
       width = 6, 
       height = 4)
dev.off()


# correlation matrix
corr_simple <- function(data=df,sig=0.5){
  #convert data to numeric in order to run correlations
  #convert to factor first to keep the integrity of the data - each value will become a number rather than turn into NA
  df_cor <- data %>% mutate_if(is.character, as.factor)
  df_cor <- df_cor %>% mutate_if(is.factor, as.numeric)
  #run a correlation and drop the insignificant ones
  corr <- cor(df_cor)
  #prepare to drop duplicates and correlations of 1     
  corr[lower.tri(corr,diag=TRUE)] <- NA 
  #drop perfect correlations
  corr[corr == 1] <- NA 
  #turn into a 3-column table
  corr <- as.data.frame(as.table(corr))
  #remove the NA values from above 
  corr <- na.omit(corr) 
  #select significant values  
  corr <- subset(corr, abs(Freq) > sig) 
  #sort by highest correlation
  corr <- corr[order(-abs(corr$Freq)),] 
  #print table
  print(corr)
  #turn corr back into matrix in order to plot with corrplot
  mtx_corr <- reshape2::acast(corr, Var1~Var2, value.var="Freq")
  
  #plot correlations visually
  corrplot(mtx_corr, is.corr=FALSE, tl.col="black", na.label=" ")
}

corr_simple(admit_train)