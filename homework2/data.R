library(janitor)
library(tidyverse)

data = read.csv('C:/Users/jofro/dev/longi/data/beta_carotene_univar.csv', header=TRUE) %>% clean_names()

data = pivot_wider(data, names_from = time, values_from = y)
colnames(data)[3:7] = c('baseline', "time1", "time2", "time3", "time4")
data = pivot_longer(data, cols = starts_with("time"), names_to = "time", values_to = 'y')
write.csv(data, 'C:/Users/jofro/dev/longi/data/beta_carotene_univar_CLEAN.csv')
