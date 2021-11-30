library(ggplot2)
library(janitor)

data = read.csv("../data_raw/month_clean.csv") %>% clean_names()
colnames(data)[1] = "neighborhood_id"

hist(data$log_crime_rate)

ggplot(data = data, aes(x = month, y = log_crime_rate, group = neighborhood_id)) +
  geom_line(color = "gray") +
  stat_smooth(aes(group = 1), method="lm", formula = y ~ poly(x, 3), se=TRUE) +
  labs(
    title = "Sphaghetti plot of crimes in neighborhoods in Denver, 2018",
    y = "Log Incidence Density Rate of Crime",
    x = "Month"
  ) +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom") +
  scale_x_continuous(breaks = c(2, 4, 6, 8, 10, 12))
  

