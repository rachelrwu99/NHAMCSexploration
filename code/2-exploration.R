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

