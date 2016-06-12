library(leaflet)
library(ggplot2)
library(dplyr)
library(data.table)
library(geosphere)

setwd("C:/Users/i076144/shadowfax_Lots-of-Locations/src")

#Reading the input Files
rider_loc = fread("../input/rider_location.csv") #173620 Records
order = fread("../input/order_data.csv")

#Some Analysis from order data
head(order)
summary(order)

#Creating some additional column
order$delivered_time = as.POSIXct(strptime(order$delivered_time,"%Y-%m-%d %H:%M:%S"))
order$scheduled_time = as.POSIXct(strptime(order$scheduled_time,"%Y-%m-%d %H:%M:%S"))

order$d_month = as.POSIXlt(order$delivered_time)$mon + 1
order$d_day = as.POSIXlt(order$delivered_time)$mday
order$d_hour = as.POSIXlt(order$delivered_time)$hour + round(as.POSIXlt(order$delivered_time)$min/60)
order$d_min = as.POSIXlt(order$delivered_time)$min

order$s_month = as.POSIXlt(order$scheduled_time)$mon + 1
order$s_day = as.POSIXlt(order$scheduled_time)$mday
order$s_hour = as.POSIXlt(order$scheduled_time)$hour + round(as.POSIXlt(order$scheduled_time)$min/60)
order$s_min = as.POSIXlt(order$scheduled_time)$min

order$del_mintues = ((as.POSIXlt(order$delivered_time)$hour * 3600 + as.POSIXlt(order$delivered_time)$min * 60 + as.POSIXlt(order$delivered_time)$sec) -
                       (as.POSIXlt(order$scheduled_time)$hour * 3600 + as.POSIXlt(order$scheduled_time)$min * 60 + as.POSIXlt(order$scheduled_time)$sec))/60

unique(order$s_day)

apply(order, 2, function(x) {length(unique(x))}) #Unique Values in Each Column

#There are two Pickup lattitude that have two seller
group_by(order,pickup_latitude) %>% summarise(diff_seller = length(unique(seller_id))) %>% filter(diff_seller > 1) 

#Trend of Orders hourly basis
with(group_by(order,s_hour) %>% summarise(tot_orders = length(order_id)),qplot(s_hour,tot_orders,geom = "line",main = "Trend of Orders hourly basis"))

#Trend of Average Delivery time hourly basis
with(group_by(order[!is.na(order$del_mintues),],s_hour) %>% summarise(avg_del_minutes = mean(del_mintues)),
     qplot(s_hour,avg_del_minutes,geom = "line",main= "Trend of Average Delivery time hourly basis"))

#Reason Why there was so much delivery Times
order[order$s_hour <= 5,]

#Group By Order Id
grp_rider = group_by(order[!is.na(order$del_mintues),],rider_id) %>% summarise(tot_orders = length(order_id),avg_del_minutes = mean(del_mintues))

with(grp_rider,qplot(rider_id,tot_orders,geom = "point",main = "Total Orders by Riders"))

#Riders Which has deliver more than Order
grp_rider[grp_rider$tot_orders > 30,]


with(grp_rider,qplot(rider_id,avg_del_minutes,geom = "point",main = "Riders average Delivery Times in Minutes"))


#There are some problem with some seller and Riders, product delivered before scheduled
order[order$del_mintues < 0,]
rider_avg = group_by(order[order$del_mintues > 0,],rider_id) %>% summarise(tot_orders = length(order_id),avg_del_minutes = mean(del_mintues))

qplot(data = rider_avg,rider_id,avg_del_minutes,main= "Riders average Delivery Times in Minutes")

#Users who have Avg deliver Time more than 50 minutes
arrange(rider_avg[rider_avg$avg_del_minutes > 50,],desc(avg_del_minutes))


#Creating Group by Seller Id
grp_seller =group_by(order,seller_id) %>% summarise(order_handled = length(order_id),rider_used = length(unique(rider_id)))
head(arrange(grp_seller,desc(rider_used)),n=10)
head(arrange(grp_seller,desc(order_handled)),n=10)

#Taking order rider Ratio
grp_seller$order_rider_ratio = grp_seller$order_handled / grp_seller$rider_used

arrange(grp_seller,desc(order_rider_ratio))

with(grp_seller,qplot(seller_id,order_handled,main="Order Handled from Sellers"))

#Take things to leaflet Map
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
  addMarkers(lng= ~delivered_longitude, lat= ~delivered_latitude, clusterOptions = markerClusterOptions(),
             popup = ~as.character(seller_id)) %>%
  # Layers control
  addLayersControl(
    overlayGroups = ~cluster_id,
    options = layersControlOptions(collapsed = TRUE)
  )
m  # Print the map
m %>% addMarkers(lng= ~pickup_longitude, lat= ~pickup_latitude, group= ~cluster_id,icon=greenLeafIcon,
                 popup = ~as.character(seller_id))



# Data Cleaning/ Preprocessing and analysis for Rider Location table
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

source("preprocess.R")
#Computation already done so no need to call
#computeAddColums()
rider_loc = fread("../input/mod_rider_loc.csv") #Reading already Modified and process data for Rider location

v_lines <- points_to_line(data = rider_loc[rider_loc$rider_id %in% tail(unique(rider_loc$rider_id),2),], 
                          long = "longitude", 
                          lat = "latitude", 
                          sort_field = "rider_seq",
                          id_field = "rider_id"
)

leaflet(data = v_lines) %>%
  addTiles() %>%
  addPolylines()

rider_loc$travel_with_order = ifelse(rider_loc$order_id < 0,0,1) #Extra column to define whether hea order while traveling or not

#Calculating Travel distance by Rider, and Whether he travel with order or not
rider_travel_dist = group_by(rider_loc, rider_id,travel_with_order) %>% summarise(tot_dist_travel = sum(distTravel)/1000 )

arrange(rider_travel_dist[rider_travel_dist$travel_with_order == 1],desc(tot_dist_travel)) #Travel with Having Order
arrange(rider_travel_dist[rider_travel_dist$travel_with_order == 0],desc(tot_dist_travel)) #Travel without Having Order

ggplot(rider_travel_dist, aes(rider_id,tot_dist_travel)) +
  geom_point()

#Finding who is traveling more
rider_travel_dist[rider_travel_dist$tot_dist_travel > 500,]

high_travel_user = rider_loc[rider_loc$rider_id %in% rider_travel_dist[rider_travel_dist$tot_dist_travel > 500,],]
high_travel_user #Detail for the User Who travell very much

order[order$rider_id == c(unique(high_travel_user$rider_id))] #User has only delivered only one Product and travel too much

#User Traveling More
leaflet(high_travel_user) %>%
  addTiles() %>%
  addMarkers(lng= ~longitude, lat= ~latitude,popup = ~as.character(order_id))

ggplot(rider_travel_dist[rider_travel_dist$tot_dist_travel < 500,], aes(rider_id,tot_dist_travel)) +
  geom_point() + ggtitle("Distance Travle by Riders")

#With Day
rider_travel_dist_day = group_by(rider_loc, rider_id,travel_with_order,day) %>% summarise(tot_dist_travel = sum(distTravel)/1000 )
head(rider_travel_dist_day)
#Plot Dist Travle Day wise  
group_by(rider_travel_dist_day[rider_travel_dist$tot_dist_travel < 500,],day) %>% summarise(tot_travel = sum(tot_dist_travel)) %>%
  ggplot(aes(day,tot_travel)) +
  geom_point() + ggtitle("Distance Travle Daily by all riders")

#Identifying Good/Efficient Drivers and Bad/In-Efficient Drivers
group_by(rider_travel_dist,travel_with_order) %>% summarise(tot_dist_travel)
with_order = rider_travel_dist[rider_travel_dist$travel_with_order ==1,] %>% select(rider_id,tot_dist_travel)
without_order = rider_travel_dist[rider_travel_dist$travel_with_order ==0,] %>% select(rider_id,tot_dist_travel)

comb_mat = merge(with_order,without_order,by = c("rider_id") )

comb_mat$overdrive =comb_mat$tot_dist_travel.y/comb_mat$tot_dist_travel.x
comb_mat=comb_mat[comb_mat$overdrive <500,]

qplot(data= comb_mat,rider_id,overdrive, main="Drive Without order per Rider")
comb_mat[comb_mat$overdrive > 50,] #Rider who Overdrive more than 50 KM

#Details For these Driver
rider_details = merge(comb_mat[comb_mat$overdrive > 50,],grp_rider, by =c("rider_id"),all.x = TRUE)
rider_details

#rmarkdown::render("Script_sf.R")
#rmarkdown::render("script_sf.R", "pdf_document")
