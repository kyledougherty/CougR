import ee 

def raster_distance_calculation(image, target, classes): 
  # Set target cell value
  reclassification_target = ee.List([target])
  # Set value of target cells equal to 1
  out = ee.List.repeat(1, reclassification_target.size())
  # Remap the image such that target cells have value of 1 
  reclassified = image.remap(reclassification_target, out)
  # Using the reclassified image, create distance layer and rename
  distance = reclassified.fastDistanceTransform(2048).sqrt().multiply(
    ee.Image.pixelArea().sqrt()).rename("Dist_" + classes[target])
  return distance
