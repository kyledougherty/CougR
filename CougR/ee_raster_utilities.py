import ee 

def ee_raster_reclassification(target_cell_value, raster_layer):
  target = ee.List(target_cell_value)
  out = ee.List.`repeat`(1, target.size())
  reclassified = raster_layer.remap(target, out)
  return(reclassified)
