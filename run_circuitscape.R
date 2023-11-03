library(tidyverse)
library(terra)

run_circuitscape <- function(model, 
                             unrescaled_model_data, 
                             model_layers, 
                             unrescaled_covariate_names, 
                             model_type = "clogit", 
                             strata_name = "StepID", 
                             strata_value, 
                             coords, 
                             output_directory, 
                             version = "4.0.5",
                             scenario = "pairwise",
                             use_included_pairs = "False",
                             point_file = "Downloads/Nodes.tif",
                             habitat_map_is_resistances = "False", 
                             connect_using_avg_resistances = "False",
                             connect_four_neighbors_only = "False",
                             low_memory_mode = "True",
                             parallelize = "False",
                             solver = "cholmod",
                             print_timings = 1,
                             preemptive_memory_release = "False",
                             print_rusages = "False",
                             max_parallel = 0,
                             mask_file = "None",
                             use_mask = "False",
                             set_null_currents_to_nodata = "False",
                             set_focal_node_currents_to_zero = "False",
                             set_null_voltages_to_nodata = "False",
                             compress_grids = "False",
                             write_cur_maps = 1,
                             write_volt_maps = 0,
                             write_cum_cur_map_only = "False",
                             log_transform_maps = "False",
                             write_max_cur_maps = "True",
                             log_level = "INFO",
                             screenprint_log = "False"){

  model_terms = str_remove(names(Prediction_Model$coefficients), ".*:")
  
  data_summary = unrescaled_model_data %>% 
    dplyr::select(dplyr::any_of(model_terms)) %>% 
    dplyr::summarise(
      dplyr::across(
        dplyr::everything(), 
        .fns = list(Mean = mean, 
                    SD = sd))) %>% 
    tidyr::pivot_longer(cols =  dplyr::everything(), 
                        names_to = "term") %>% 
    dplyr::mutate(Statistics = stringr::str_extract(term, "Mean|SD"), 
                  term = stringr::str_remove(term, "_Mean|_SD")) %>% 
    tidyr::pivot_wider(values_from = value, 
                       names_from = Statistics) %>% 
    dplyr::group_by(term) %>% 
    dplyr::group_split() %>% 
    purrr::set_names(purrr::map(., 
                                ~unique(.x$term)))
  
  rescaled_rasters = purrr::map(names(model_layers),
                                function(layer){
                                  
                                  if(layer %in% unrescaled_covariate_names){
                                    
                                    model_layers[[layer]]
                                    
                                  } else{
                                    
                                    (model_layers[[layer]] - data_summary[[layer]]$Mean)/
                                      (2*data_summary[[layer]]$SD)
                                    
                                  }
                                  
                                }) %>%
    terra::rast()

  if(model_type == "clogit"){
    
    predicted_probability = terra::predict(rescaled_rasters, 
                                           model,
                                           fun = function(m, d){
                                             predict(m, 
                                                     d, 
                                                     type = "risk",
                                                     reference = "sample")
                                           },
                                           const = dplyr::tibble(!!strata_name := strata_value))
    
  }
  
  resistance = 1 - predicted_probability
  
  nodes = dplyr::tibble(LAT = coords$lat, 
                        LONG = coords$long) %>%
    sf::st_as_sf(coords = c("LONG", "LAT"), 
                 crs = 4326) %>%
    sf::st_buffer(15) %>% 
    sf::st_as_sf() %>% 
    sf::st_transform(sf::st_crs(model_layers)) %>%
    terra::vect() %>%
    terra::rasterize(model_layers, field = 1:nrow(.))
  
  dir.create(output_directory)
  predicted_probability_path = paste0(output_directory, "/Predicted_Probability_Surface.tif")
  terra::writeRaster(predicted_probability, 
                     predicted_probability_path)
  resistance_path = paste0(output_directory, "/Resistance_Surface.tif")
  terra::writeRaster(resistance, 
                     resistance_path)
  node_path = paste0(output_directory, "/Nodes.tif")
  terra::writeRaster(nodes, 
                     node_path)
  
  if(habitat_map_is_resistances == TRUE){
    habitat_file = resistance
  } else{ 
    habitat_file = predicted_probability_path
  }

  write_lines(paste(paste0("[Version]\n",
                           "version = ", version),
                    paste0("[Circuitscape mode]",
                           "data_type = ", data_type, "\n",
                           "scenario = ", scenario),
                    paste0("[Options for pairwise and one-to-all and all-to-one modes]\n",
                           "use_included_pairs = ", use_included_pairs, "\n",
                           "point_file = ", paste0(output_directory, "/Nodes.tif")),
                    paste0("[Habitat raster or graph]\n",
                           "habitat_map_is_resistances = ", habitat_map_is_resistances, "\n",
                           "habitat_file = ", habitat_file),
                    paste0("[Connection scheme for raster habitat data]\n",
                           "connect_using_avg_resistances = ", connect_using_avg_resistances, "\n",
                           "connect_four_neighbors_only = ", connect_four_neighbors_only),
                    paste0("[Calculation options]\n",
                           "low_memory_mode = ", low_memory_mode, "\n",
                           "parallelize = ", parallelize, "\n",
                           "solver = ", solver, "\n",
                           "print_timings = ", print_timings, "\n",
                           "preemptive_memory_release = ", preemptive_memory_release, "\n",
                           "print_rusages = ", print_rusages, "\n",
                           "max_parallel = ", max_parallel),
                    paste0("[Mask File]\n",
                           "mask_file = ", mask_file, "\n",
                           "use_make = ", use_mask),
                    paste0("[Output options]\n",
                           "set_null_currents_to_nodata = ", set_null_currents_to_nodata, "\n",
                           "set_focal_node_currents_to_zero = ", set_focal_node_currents_to_zero, "\n",
                           "set_null_voltages_to_nodata = ", set_null_voltages_to_nodata, "\n",
                           "compress_grids = ", compress_grids, "\n",
                           "write_cur_maps = ", write_cur_maps, "\n",
                           "write_volt_maps = ", write_volt_maps, "\n",
                           "output_file = ", paste0(output_directory, "/out.out"), "\n",
                           "write_cum_cur_map_only = ", write_cum_cur_map_only, "\n",
                           "log_transform_maps = ", log_transform_maps, "\n",
                           "write_max_cur_maps = ", write_max_cur_maps),
                    paste0("[Logging Options]\n",
                           "log_level = ", log_level, "\n",
                           "log_file = ", paste0(output_directory, "/out.log"), "\n",
                           "profiler_log_file = ", paste0(output_directory, "/out_rusages.out"), "\n",
                           "screenprint_log = ", screenprint_log),
                    sep = "\n\n"),
              paste0(output_directory, "/Circuitscape_Initialization.ini"))
  
  if(JuliaCall::julia_installed_package("Circuitscape") == "nothing"){
    JuliaCall::julia_command('Pkg.add("Circuitscape")')
  }

  JuliaCall::julia_command(paste0("cd(", output_directory, ")"))
  JuliaCall::julia_command('using Circuitscape')
  JuliaCall::julia_command('compute("Circuitscape_Initialization.ini")')
  
}