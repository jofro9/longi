library(dplyr)
library(ggmap)
library(ggplot2)
library(janitor)
library(lubridate)
library(RColorBrewer)
library(sf)
library(terra)
library(tidyverse)

data = read.delim("../data_raw/denver.txt")
pop = read.csv("../data_raw/census_neighborhood_demographics_2010.csv", header = TRUE) %>% clean_names()

# read in shapes
denver_boundary = st_read("../shapes/statistical_neighborhoods.shp") %>% clean_names()
denver_boundary$neighborhood_id = tolower(denver_boundary$nbhd_name)
denver_boundary$neighborhood_id = gsub(" - ", "-", denver_boundary$neighborhood_id)
denver_boundary[denver_boundary$neighborhood_id == "lincoln park",]$neighborhood_id = "lincoln-park"
denver_boundary$count = count(data, neighborhood_id)$n

# populations
pop$nbrhd_name = gsub(" / ", " - ", pop$nbrhd_name)
pop[15, ]$nbrhd_name = "Central Park"
denver_boundary = denver_boundary %>% left_join(pop[,2:3], by = c("nbhd_name" = "nbrhd_name"))

# rates
denver_boundary$crime_rate = denver_boundary$count / (denver_boundary$population_2010)
denver_boundary$above_median_rate = vector("double", dim(denver_boundary)[1])
denver_boundary$above_median_rate = 0
for (i in 1:dim(denver_boundary)[1]) {
  if (denver_boundary$crime_rate[i] > median(denver_boundary$crime_rate)) {
    denver_boundary$above_median_rate[i] = 1
  }
}

denver_boundary$above_median_rate = as.factor(denver_boundary$above_median_rate)

# logistic regression with random intercept for neighborhood odds of crime vs traffic incident
log_reg = glmer(cbind(is_crime, is_traffic) ~ log10(pop) + (1 | neighborhood_id), family = binomial(link = "logit"), data=data)
log_summary = summary(log_reg)
random_effects = exp(ranef(log_reg)$neighborhood_id) # odds ratio

denver_boundary = denver_boundary[order(denver_boundary$neighborhood_id),]
denver_boundary$odds_ratio = random_effects[,1]

# map
ggplot() + 
  geom_sf(data = denver_boundary, aes(fill = odds_ratio), size = 0.5, color="gray") +
  labs(
    title="Odds Ratio of Crimes to traffic incidents by Neighborhood, Denver 2018",
    fill="Odds Ratio"
  ) +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom") +
  scale_fill_distiller(type="seq", palette = "Spectral")
