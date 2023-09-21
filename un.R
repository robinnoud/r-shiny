install.packages(c("httr","jsonlite","RMySQL"))

library(httr)
library(jsonlite)
library(RMySQL)

data <- GET("https://api.jcdecaux.com/vls/v3/stations?contract=Lyon&apiKey=f584928eea4d8fdf00e5a98059f92b70724f917b")
# dataframe
data_list  <- fromJSON(rawToChar(data$content), flatten = TRUE)

con <- dbConnect(MySQL(),
                 user = "sql11646641",
                 password = "luvJnM2nll",
                 host = "sql11.freesqldatabase.com",
                 dbname = "sql11646641")
#dbGetInfo(con)
#dbListTables(con)
#dbListFields(con)

#dbGetQuery(con, )
dbWriteTable(con,"rShiny",data_list)

table_stations = data_list[,c(1,2,3,4,12,13)]
table_etat = data_list[,c(1,5,6,7,9,10)]
#table_communes = data_list[,c(1)]

#########################################
install.packages('tidygeocoder')
#library(dplyr, warn.conflicts = FALSE)
library(tidygeocoder)
#reverse_geocode(lat = 45.77624, long = 4.871634, method = 'osm',full_results = TRUE)
#geocode( method = 'osm', lat = 45.77624 , long = 4.871634)
########################################
options(tidygeocoder.progress_bar = FALSE)
adrs = reverse_geo(lat = 45.77624, long = 4.871634, method = "osm")[3]
adrs =as.character(adrs)
# prendre le 3 ème 
strsplit(adrs, ",")
