library(dplyr)
library(ggmap)
library(ggplot2)
library(janitor)
library(lubridate)
library(sf)
library(terra)
library(tidyverse)


# https://www.kaggle.com/paultimothymooney/denver-crime-data/version/126?select=crime.csv
data = read.csv("../data_raw/crime.csv", header=TRUE) %>% clean_names()
pop = read.csv("../data_raw/census_neighborhood_demographics_2010.csv", header = TRUE) %>% clean_names()

data = data %>%
  filter(
    mdy_hms(data$first_occurrence_date) > as.POSIXct("2019-01-01 00:00:00", tz="UTC"), mdy_hms(data$first_occurrence_date) < as.POSIXct("2019-12-31 11:59:59 PM", tz="UTC")
  )

data = data[!is.na(data$neighborhood_id),]

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

# Diverging Barcharts
ggplot(denver_boundary, aes(x=reorder(nbhd_name, crime_rate), y=crime_rate - median(crime_rate), label="Crimes per 1000 people")) + 
  geom_bar(stat='identity', aes(fill=above_median_rate), position = position_dodge(width = 1), width = 0.5) + 
  coord_flip() +
  labs(
    title="Normalized Crime Incidence Density Rates in Denver, by Neighborhood, 2019",
    y="Incidence Density Rate",
    x="Neighborhood",
    fill=""
  ) +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(labels=c("Below Median", "Above Median"), values = c("green", "red"))
  
# map
ggplot() + 
  geom_sf(data = denver_boundary, aes(fill = crime_rate), size = 0.5, color="gray") +
  labs(
    title="Crime Rates by Neighborhood, Denver 2019",
    fill="Incidence Density Rate"
  ) +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom") +
  scale_fill_gradient(
    low = "yellow",
    high = "red",
    space = "Lab",
    na.value = "grey50",
    guide = "colourbar",
    aesthetics = "fill"
  )

# areas
shapes = vect('../shapes/statistical_neighborhoods.shp')
shapes$areas = expanse(shapes) / 1000000

newdata = cbind(denver_boundary$nbhd_id, denver_boundary$nbhd_name, denver_boundary$count, denver_boundary$population_2010, denver_boundary$crime_rate, denver_boundary$above_median_rate)
write.table(newdata, "../data_raw/denver_boundary.txt", sep="\t")