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
