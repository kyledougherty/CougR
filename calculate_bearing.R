calculate_bearing <- function(initial_lat, initial_long, final_lat, final_long, type = c("Degrees_180", "Degrees_360", "Radians")){
  # Note that the bearings obtained here are (within negligible rounding error) 
  # equivalent to those obtained by geosphere
  
  # First, conver the LAT/LONG coordinates to radians
  final_lat = final_lat * pi/180
  final_long = final_long * pi/180
  initial_lat = initial_lat * pi/180
  initial_long = initial_long * pi/180
  
  # Derive a bearing in radians using the haversine formula
  bearing_rad = atan2(sin(final_long-initial_long) * cos(final_lat), 
                      cos(initial_lat) * sin(final_lat) - sin(initial_lat)*cos(final_lat)*cos(final_long - initial_long))
  
  # If desired, output the bearing in radians
  if(type == "Radians"){
    bearing_rad
  } else if(type == "Degrees_180"){
    # If type is set to Degrees_180, the bearing will be bounded between 
    # -180 and 180. This is the behavior of geosphere::bearing()
    bearing_rad * 180/pi
  } else if(type == "Degrees_360"){
    # If type is set to Degrees_360, the bearing will be bounded between 
    # 0 and 360. This is the behavior of geosphere::bearingRhumb()
    (bearing_rad*180/pi + 360) %% 360
  }
  
}