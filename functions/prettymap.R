base_map = function(regions = read_regions(),
                    ghcn = NULL,
                    buoys = buoy_lut(),
                    usgs = usgs_lut(),
                    inflate = 0.1){
  
  
  if (FALSE){
    regions = read_regions()
    ghcn = NULL
    buoys = buoy_lut()
    usgs = usgs_lut()
    inflate = 0.1
  }
  bb = sf::st_bbox(regions) |>
    as.vector()
  bb = bb + inflate*c(-1,-1,1,1)

    
  ggOceanMaps::basemap(data = regions, bathymetry = TRUE) + 
    geom_polygon(data = regions, 
                 color = "red", fill = NA)
  
}