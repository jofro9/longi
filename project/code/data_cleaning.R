library(dplyr)
library(janitor)
library(lubridate)
library(tidyverse)

# https://www.kaggle.com/paultimothymooney/denver-crime-data/version/126?select=crime.csv
data = read.csv("../data_raw/crime.csv", header=TRUE) %>% clean_names()
data$reported_date = as.POSIXct(parse_date_time(data$reported_date, '%m/%d/%Y %I:%M:%S %p'))

pop = read.csv("../data_raw/census_neighborhood_demographics_2010.csv", header = TRUE) %>% clean_names()

pop = pop[, 1:3]

data = data %>%
  filter(
    as.POSIXct(data$reported_date) > as.POSIXct("2018-01-01 00:00:00", tz="UTC"), as.POSIXct(data$reported_date) < as.POSIXct("2018-12-31 11:59:59 PM", tz="UTC")
  )

data = data[!is.na(data$neighborhood_id),]
pop$nbrhd_name = tolower(pop$nbrhd_name)
pop$nbrhd_name = gsub(" / ", "-", pop$nbrhd_name)
pop$nbrhd_name = gsub(" - ", "-", pop$nbrhd_name)
pop$nbrhd_name = gsub(" ", "_", pop$nbrhd_name)
pop$nbrhd_name = gsub("-", "_", pop$nbrhd_name)
data$neighborhood_id = gsub(" ", "_", data$neighborhood_id)
data$neighborhood_id = gsub("-", "_", data$neighborhood_id)

data$pop = vector("double", dim(data)[1])
data$pop = 0

for (i in 1:dim(data)[1]) {
  for (j in 1:dim(pop)[1]) {
    if (data$neighborhood_id[i] == pop$nbrhd_name[j]) {
      data$pop[i] = pop$population_2010[j]
    }
  }
}

write.table(data, "../data_raw/denver.txt", sep="\t")

# time analysis
month = data %>%
  group_by(neighborhood_id, month = floor_date(as.POSIXct(reported_date), "months")) %>%
  summarize(count = n(), .groups="rowwise")

month$pop = vector('double', dim(month)[1])
month$pop = 0

names = unique(data$neighborhood_id)
names = names[order(names)]
pop = pop[order(pop$nbrhd_name),]

for (i in 1:dim(month)[1]) {
  for (j in 1:dim(pop)[1]) {
    if (month$neighborhood_id[i] == names[j]) {
      month$pop[i] = pop$population_2010[j]
    }
  }
}

month$log_crime_rate = log(month$count / month$pop)

write.table(month, "../data_raw/month.txt", sep="\t")

# days
day = data %>%
  group_by(neighborhood_id, day = floor_date(as.POSIXct(reported_date), "days")) %>%
  summarize(count = n(), .groups="rowwise")

day$pop = vector('double', dim(day)[1])
day$pop = 0

for (i in 1:dim(day)[1]) {
  for (j in 1:dim(pop)[1]) {
    if (day$neighborhood_id[i] == names[j]) {
      day$pop[i] = pop$population_2010[j]
    }
  }
}

day$log_crime_rate = log(day$count / day$pop)

write.table(day, "../data_raw/day.txt", sep="\t")

