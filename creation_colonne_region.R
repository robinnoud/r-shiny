library(tidygeocoder)
library(httr)
library(jsonlite)
data <- GET("https://api.jcdecaux.com/vls/v3/stations?contract=Lyon&apiKey=f584928eea4d8fdf00e5a98059f92b70724f917b")
# dataframe
data_list  <- fromJSON(rawToChar(data$content), flatten = TRUE)
data_list$region <- "inconnue"
for (i in 1:nrow(data_list)) {
  result <- reverse_geo(lat = data_list[i, "position.latitude"], long = data_list[i, "position.longitude"], method = "osm")
  result <- as.character(result[3])
  result <- strsplit(result, ",")
  data_list$adrs[i] <- result
}