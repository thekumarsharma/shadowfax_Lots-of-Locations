#library(ggmap)
library(leaflet)
library(ggplot2)
#library(plyr)
library(dplyr)
library(data.table)
library(geosphere)

setwd("C:/Data/Personal/Study/Kaggle/shadowfax/src")

#Reading the input Files
rider_loc = fread("../input/rider_location.csv") #173620 Records
order = fread("../input/order_data.csv")

rider_loc$update_timestamp = as.POSIXct(strptime(rider_loc$update_timestamp,"%m/%d/%Y %H:%M"))
rider_loc$day = as.POSIXlt(rider_loc$update_timestamp)$mday
rider_loc$mon = as.POSIXlt(rider_loc$update_timestamp)$mon +1
rider_loc$year = as.POSIXlt(rider_loc$update_timestamp)$year + 1900
rider_loc$minutes = as.POSIXlt(rider_loc$update_timestamp)$hour * 60 + as.POSIXlt(rider_loc$update_timestamp)$min
rider_loc$hour = as.POSIXlt(rider_loc$update_timestamp)$hour + round(as.POSIXlt(rider_loc$update_timestamp)$min/60)

#Removing Some columns
rider_loc$source = NULL
rider_loc$V7 = NULL
rider_loc$update_timestamp = NULL

#Some Analysis From Rider Location File
head(rider_loc)
summary(rider_loc)

apply(rider_loc, 2, function(x) {length(unique(x))}) #Unique Values in Each Column

table(rider_loc$year) #There are 5 Record from 2010 Year as well so we are removing them as they may be not relevent

rider_loc = rider_loc[rider_loc$year == 2016,] #Keeping only 2016 Records

table(rider_loc$day) #Again some descrepency we will remove data for 2,6,11
rider_loc = rider_loc[rider_loc$day %in% c(3,4,5),] #Keeping only 3,4,5 th day Records

#apply(rider_loc, 2, function(x) {length(unique(x))}) #Unique Values in Each Column
distm (c(12.9584737, 77.6397935), c(12.9577956, 77.6413886), fun = distHaversine)
rider_seq =NULL
for(i in unique(rider_loc$rider_id)){
  rider_seq = append(rider_seq,seq(1:nrow(rider_loc[rider_loc$rider_id == i,])))
}
rider_loc$rider_seq = rider_seq

for(i in 1:nrow(rider_loc)){
  if(i == 1 | rider_loc[i,c("rider_seq")] == 1){
    #if(i == 1){
    rider_loc$distTravel[i] = 0
  }else if(i== nrow(rider_loc) | rider_loc[i+1,c("rider_seq")] == 1){
    #}else if(i== nrow(full_reshape)){
    rider_loc$distTravel[i] = 0
  }else{
    rider_loc$distTravel[i] = distm(c(rider_loc$latitude[i],rider_loc$longitude[i]), c(rider_loc$latitude[i+1],rider_loc$longitude[i+1]))
  }
}


#Some basic plot from rider_location
m <- leaflet(rider_loc) %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(lng= ~longitude, lat= ~latitude, popup="The birthplace of R")
m  # Print the map


#Some Analysis from order data
head(order)
summary(order)

order$delivered_time = as.POSIXct(strptime(order$delivered_time,"%Y-%m-%d %H:%M:%S"))
order$scheduled_time = as.POSIXct(strptime(order$scheduled_time,"%Y-%m-%d %H:%M:%S"))

order$d_month = as.POSIXlt(order$delivered_time)$mon + 1
order$d_day = as.POSIXlt(order$delivered_time)$mday
order$d_hour = as.POSIXlt(order$delivered_time)$hour + round(as.POSIXlt(order$delivered_time)$min/60)

order$s_month = as.POSIXlt(order$scheduled_time)$mon + 1
order$s_day = as.POSIXlt(order$scheduled_time)$mday
order$s_hour = as.POSIXlt(order$scheduled_time)$hour + round(as.POSIXlt(order$scheduled_time)$min/60)

order$del_mintues = ((as.POSIXlt(order$delivered_time)$hour * 3600 + as.POSIXlt(order$delivered_time)$min * 60 + as.POSIXlt(order$delivered_time)$sec) -
  (as.POSIXlt(order$scheduled_time)$hour * 3600 + as.POSIXlt(order$scheduled_time)$min * 60 + as.POSIXlt(order$scheduled_time)$sec))/60

unique(order$s_day)

apply(order, 2, function(x) {length(unique(x))}) #Unique Values in Each Column

table(order$s_day)

options(dplyr.print_min = 70)
group_by(order,seller_id) %>% summarise(length(unique(pickup_latitude)))
group_by(order,pickup_latitude) %>% summarise(diff_seller = length(unique(seller_id))) %>% filter(diff_seller > 1)

greenLeafIcon <- makeIcon(
  iconUrl = "http://leafletjs.com/docs/images/leaf-green.png",
  iconWidth = 20, iconHeight = 35,
  iconAnchorX = 22, iconAnchorY = 34,
  shadowUrl = "http://leafletjs.com/docs/images/leaf-shadow.png",
  shadowWidth = 20, shadowHeight = 24,
  shadowAnchorX = 4, shadowAnchorY = 22
)


m <- leaflet(order) %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  # Add tiles as baseGroup
  
  addMarkers(lng= ~delivered_longitude, lat= ~delivered_latitude, clusterOptions = markerClusterOptions(),
             popup = ~as.character(seller_id)) %>%
  # Layers control
  addLayersControl(
    overlayGroups = ~cluster_id,
    options = layersControlOptions(collapsed = FALSE)
  )
m  # Print the map
m %>% addMarkers(lng= ~pickup_longitude, lat= ~pickup_latitude, group= ~cluster_id,icon=greenLeafIcon,
           popup = ~as.character(seller_id))

#Creating Group by Seller Id
grp_seller =group_by(order,seller_id) %>% summarise(order_handled = length(unique(order_id)),rider_used = length(unique(rider_id)))
  
head(arrange(grp_seller,desc(rider_used)),n=10)
head(arrange(grp_seller,desc(order_handled)),n=10)

