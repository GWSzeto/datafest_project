#Reading data with fread
library(ggplot2)
library(data.table)
library(stringr)
library(ggmap)
library(maptools)
library(maps)
library(RColorBrewer)

data <- fread("data.txt")
dest <-fread("dest.txt")
str(data)
str(dest)


#Geting a random sampling of data - for modelling later

set.seed(1234)
data_sample_index <- sample(1:nrow(data), 1000)
data_sample<-data[data_sample_index,]

#Transforming to factor type

data_sample$site_name <- as.factor(data_sample$site_name)
data_sample$user_location_country <- as.factor(data_sample$user_location_country)
data_sample$user_location_region <- as.factor(data_sample$user_location_region)
data_sample$user_location_city <- as.factor(data_sample$user_location_city)
data_sample$hotel_country <- as.factor(data_sample$hotel_country)
data_sample$distance_band <- as.factor(data_sample$distance_band)
data_sample$hist_price_band <- as.factor(data_sample$hist_price_band)
data_sample$popularity_band <- as.factor(data_sample$popularity_band)
  
#Comparing levels of two factors

setdiff( levels(data_sample$hotel_country), levels(data_sample$user_location_country))
levels(data_sample$user_location_country)

# barplot for site names in data_sample
barplot(table(data_sample[,c("site_name")]))
ggp <- ggplot(data.frame(data_sample$site_name), aes(x=data_sample$site_name))
ggp + geom_bar() + coord_flip()

#filtering for all the travels done in 2015
data_sample[with (data_sample, grepl("2015", date_time))]

#changing all the NULLS to NA then making it numeric
data_sample$orig_destination_distance[data_sample$orig_destination_distance == "NULL"] <- "NA"
data_sample$user_location_latitude[data_sample$user_location_latitude == "NULL"] <- "NA"
data_sample$user_location_longitude[data_sample$user_location_longitude == "NULL"] <- "NA"
dest$srch_destination_latitude[dest$srch_destination_latitude == "NULL"] <- "NA"
dest$srch_destination_longitude[dest$srch_destination_longitude == "NULL"] <- "NA"
dest$srch_destination_latitude <- as.numeric(dest$srch_destination_latitude)
dest$srch_destination_longitude <- as.numeric(dest$srch_destination_longitude)
data_sample$orig_destination_distance <- as.numeric(data_sample$orig_destination_distance)
data_sample$user_location_latitude <- as.numeric(data_sample$user_location_latitude)
data_sample$user_location_longitude <- as.numeric(data_sample$user_location_longitude)

#finding how long the stay using the difference between check in and check out
data_ci <- data_sample$srch_ci
data_co <- data_sample$srch_co
data_date_diff <- as.Date(data_co, format="%Y-%m-%d") - as.Date(data_ci, format="%Y-%m-%d")

#mapping the world and creating a heatmap densities on it with desitnation longitude and latitudes
data_canada <- data[with (data, grepl("CANADA", user_location_country))]
data_canada_dest <- merge(x = data_canada, y = dest[, c("srch_destination_id","srch_destination_latitude", "srch_destination_longitude")], by = "srch_destination_id", all.x=TRUE)
data_map <- data_canada_dest[, c("srch_destination_id", "date_time", "srch_destination_latitude", "srch_destination_longitude")]
data_map <- na.omit(data_map)
data_map$date_time <- as.Date(data_map$date_time, format="%Y-%m-%d")

dates <- unique(data_map$date_time)
dates <- sort(dates)

data_x <- data_map[with (data_map, grepl("2015-03-31", date_time))]$srch_destination_longitude
data_y <- data_map[with (data_map, grepl("2015-03-31", date_time))]$srch_destination_latitude

mapWorld <- borders("world", colour="gray50", fill="gray50") 
mp <- ggplot() + mapWorld + geom_point(aes(x=data_x, y=data_y), color="red", size=1) + ggtitle("2015-03-31")


# function that takes all the booking coordinates coressponding to that date and plots them
mapping <- function(x){
  longitude <- data_map[with (data_map, grepl(x, date_time))]$srch_destination_longitude
  latitude <- data_map[with (data_map, grepl(x, date_time))]$srch_destination_latitude
  
  mp <- ggplot() + mapWorld + geom_point(aes(x=longitude, y=latitude), color="red", size=1)
  #mp <- mp + stat_density2d(aes(x=data_x, y=data_y, fill = ..level..), alpha=0.5, geom="polygon") + scale_fill_gradientn(colours=rev(brewer.pal(11,"Spectral")))
}

# iterates through every day in 2015 and plots the booking coordinates on a world map
for (i in 1:NROW(dates)){
  longitude <- data_map[with (data_map, grepl(dates[i], date_time))]$srch_destination_longitude
  latitude <- data_map[with (data_map, grepl(dates[i], date_time))]$srch_destination_latitude
  
  file_name <- file.path("~", "projects", "datafest", "images", paste("map", i, ".png", sep=""))
  png(filename = file_name, width=800)
  print(ggplot() + mapWorld + geom_point(aes(x=longitude, y=latitude), color="red", size=1))
  dev.off()
}




