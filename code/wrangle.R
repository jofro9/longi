library(dplyr)
library(ggmap)
library(ggplot2)
library(janitor)
library(lubridate)
library(tidyverse)
library(sf)

data = read.csv("../data_raw/crime.csv", header=TRUE) %>% clean_names()

data = data %>%
  filter(
    mdy_hms(data$first_occurrence_date) > as.POSIXct("2019-01-01 00:00:00", tz="UTC"), mdy_hms(data$first_occurrence_date) < as.POSIXct("2019-12-31 11:59:59 PM", tz="UTC")
  )

data = data[!is.na(data$neighborhood_id),]

# histogram of the data
# Manual levels
neighborhood_table = table(data$neighborhood_id)
neighborhood_levels = names(neighborhood_table)[order(neighborhood_table)]
data$neighborhood_id2 = factor(data$neighborhood_id, levels = neighborhood_levels)

ggplot(data, aes(neighborhood_id2)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 90), plot.title = element_text(hjust = 0.5)) +
  ggtitle("Bar plot of counts of crimes by neighborhood, Denver County 2020")

# read in shapes
denver_boundary = st_read("../shapes/statistical_neighborhoods.shp") %>% clean_names()
denver_boundary$neighborhood_id = tolower(denver_boundary$nbhd_name)
denver_boundary$neighborhood_id = gsub(" - ", "-", denver_boundary$neighborhood_id)
denver_boundary[denver_boundary$neighborhood_id == "lincoln park",]$neighborhood_id = "lincoln-park"
denver_boundary$count = count(data, neighborhood_id)$n

# create bins
bins = quantile(denver_boundary$count)
names = c("0-25%", "25-50%", "50-75%", "75-100%")
denver_boundary$count_bins = cut(denver_boundary$count, breaks = bins, labels = names, include.lowest = TRUE)
colnames(denver_boundary)[8] = "Crime Quantiles"

ggplot() + 
  geom_sf(data = denver_boundary, aes(fill = `Crime Quantiles`), size = 0.5, color = "gray") +
  ggtitle("Crimes by Neighborhood, Denver 2019") +
  scale_fill_brewer(palette = "YlOrRd") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom")
