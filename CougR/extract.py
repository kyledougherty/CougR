import ee

def systematic_sample(polygon, layers, scale, folder, file_name): 
  
  image = layers.clip(polygon)
  
  lat_long = image.pixelLonLat()
  
  coordinates = lat_long.select(["longitude", "latitude"]).reduceRegion(
    reducer = ee.Reducer.toList(),
    geometry = polygon.geometry(),
    scale = scale,
    maxPixels = 1e13)
    
  Lat = ee.List(coordinates.get("latitude"))
  
  Long = ee.List(coordinates.get("longitude"))
  
  point_list = Long.zip(Lat)
  
  Sampled_Points = []
  for offset in list(range(0, point_list.size().getInfo(), 10000)): 
    point_grid = ee.FeatureCollection(point_list.slice(offset, offset + 10000).map(lambda point:
      ee.Feature(ee.Geometry.Point(point)).set(polygon.toDictionary())))
    
    sampled_points = layers.reduceRegions(collection = point_grid, reducer = ee.Reducer.first())
    Sampled_Points.append(sampled_points) 
  
  return ee.batch.Export.table.toDrive(
    collection = ee.FeatureCollection(Sampled_Points).flatten(),
    description = file_name,
    folder = folder,
    fileNamePrefix = file_name).start()
    
def sample_points(points, layers, scale, file_name): 
  
  n_points = points.size().getInfo()
  
  point_list = points.toList(n_points)
  
  Sampled_Points = []
  for offset in list(range(0, n_points, 10000)): 
    point_subset = ee.FeatureCollection(point_list.slice(offset, offset + 10000))
    
    sampled_points = layers.reduceRegions(
      collection = point_subset, 
      reducer = ee.Reducer.first(), 
      scale = scale)
     
    Sampled_Points.append(sampled_points)
  
  return ee.batch.Export.table.toDrive(
    collection = ee.FeatureCollection(Sampled_Points).flatten(),
    description = file_name,
    fileNamePrefix = file_name).start()
