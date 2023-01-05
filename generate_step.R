generate_step <- function(initial_lat, initial_long, bearing, distance_meters){
  
  # Convert the initial LAT/LONG coordinates 
  # and the bearing to radians
  initial_lat = initial_lat * pi/180
  initial_long = initial_long * pi/180
  bearing_rad = bearing * pi/180
  
  # Convert the step length to radians (6371000 represents
  # the diameter of the earth)
  distance_radians = distance_meters/6371000
  
  # Calculate destination point based on haversine formula
  final_lat = asin(sin(initial_lat) * cos(distance_radians) + 
                     cos(initial_lat) * sin(distance_radians) * cos(bearing_rad))
  final_long = initial_long + atan2(sin(bearing_rad) * sin(distance_radians) * cos(initial_lat), 
                                    cos(distance_radians) - sin(initial_lat)*sin(final_lat))
  
  # Convert the destination point from radians to LAT/LONG
  return(c(LAT = final_lat * 180/pi, 
           LONG = final_long * 180/pi))
}