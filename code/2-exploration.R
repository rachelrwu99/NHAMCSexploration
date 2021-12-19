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