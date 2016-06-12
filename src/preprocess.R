library(sp)
library(maptools)

points_to_line <- function(data, long, lat, id_field = NULL, sort_field = NULL) {
  
  # Convert to SpatialPointsDataFrame
  coordinates(data) <- c(long, lat)
  
  # If there is a sort field...
  if (!is.null(sort_field)) {
    if (!is.null(id_field)) {
      data <- data[order(data[[id_field]], data[[sort_field]]), ]
    } else {
      data <- data[order(data[[sort_field]]), ]
    }
  }
  
  # If there is only one path...
  if (is.null(id_field)) {
    
    lines <- SpatialLines(list(Lines(list(Line(data)), "id")))
    
    return(lines)
    
    # Now, if we have multiple lines...
  } else if (!is.null(id_field)) {  
    
    # Split into a list by ID field
    paths <- sp::split(data, data[[id_field]])
    
    sp_lines <- SpatialLines(list(Lines(list(Line(paths[[1]])), "line1")))
    
    # I like for loops, what can I say...
    for (p in 2:length(paths)) {
      id <- paste0("line", as.character(p))
      l <- SpatialLines(list(Lines(list(Line(paths[[p]])), id)))
      sp_lines <- spRbind(sp_lines, l)
    }
    
    return(sp_lines)
  }
}

computeAddColums <- function(){
  #Identifying Order which is carrying by person
  filtered_oreder = order[!is.na(order$del_mintues),]
  rider_loc$order_id = rep(-1, nrow(rider_loc))
  for(i in 1:nrow(filtered_oreder)){
    temp = rider_loc[rider_loc$rider_id == filtered_oreder$rider_id[i] & rider_loc$day == filtered_oreder$s_day[i] & rider_loc$minutes > ((filtered_oreder$s_hour[i])*60 + filtered_oreder$s_min[i])
                     & rider_loc$minutes < ((filtered_oreder$d_hour[i])*60 + filtered_oreder$d_min[i]),] 
    rider_loc[rider_loc$id %in% temp$id,c("order_id")]= rep(filtered_oreder$order_id[i],nrow(temp))
    #filtered_oreder$order_id = rep(filtered_oreder$rider_id[i],nrow(temp))
  }
  length(unique(order$order_id))
  table(order$d_day)
  
  rider_seq =NULL
  for(i in unique(rider_loc$rider_id)){
    rider_seq = append(rider_seq,seq(1:nrow(rider_loc[rider_loc$rider_id == i,])))
  }
  rider_loc$rider_seq = rider_seq
  
  for(i in 1:nrow(rider_loc)){
    if(i == 1 | rider_loc[i,c("rider_seq")] == 1){
      #if(i == 1){
      rider_loc$distTravel[i] = 0
    }else if(i== nrow(rider_loc) | rider_loc[i+1,c("rider_seq")] == 1 | (rider_loc[i+1,day] - rider_loc[i,day]) != 0){
      #}else if(i== nrow(full_reshape)){
      rider_loc$distTravel[i] = 0
    }else{
      rider_loc$distTravel[i] = distm(c(rider_loc$latitude[i],rider_loc$longitude[i]), c(rider_loc$latitude[i+1],rider_loc$longitude[i+1]))
    }
    if(0 == i%%10000){
      print(i)
    }
    
  }
  write.csv(rider_loc, "../input/mod_rider_loc.csv",row.names = FALSE)
  
}