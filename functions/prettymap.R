mar_map = function(){
  if (FALSE){
    regions = read_regions()# |>
    #mutate(name = factor(name)) |>
    #group_by(region)
    ghcn = ghcn_lut()
    buoys = buoy_lut()
    usgs = usgs_lut()
    inflate = 0.1
  }
  bb = sf::st_bbox(regions) |>
    as.vector()
  bb = bb + inflate*c(-1,-1,1,1)
  
  library(marmap)
  base <- getNOAA.bathy(lon1 = bb[1], lon2 = bb[3],
                          lat1 = bb[2], lat2 = bb[4], 
                          resolution = 2)
  
  blues <- c("lightsteelblue4", "lightsteelblue3", "lightsteelblue2", "lightsteelblue1")
  # Plotting the bathymetry with different colors for land and sea
  plot(base, 
       image = TRUE, 
       land = TRUE, 
       lwd = 0.1, 
       bpal = list(c(0, max(base), "grey"),
                    c(min(base),0, blues)))
  
  
  plot(base, deep = 0, shallow = 0, step = 0, lwd = 0.4)
}





#' Plot using ggOceanMaps

base_map = function(regions = read_regions(),
                    ghcn = ghcn_lut(),
                    buoys = NULL, #buoy_lut(form = 'sf'),
                    usgs = usgs_lut(form = 'sf')){
  
  
  if (FALSE){
    regions = read_regions()
    ghcn = ghcn_lut()
    buoys = buoy_lut(form = 'sf')
    usgs = usgs_lut(form = 'sf')
  }
  
  bb = sf::st_bbox(regions) |>
    as.vector()
  bb = bb[c(1,3,2,4)] + c(-.4, 0.2, -0.1, .2)
  
  
  
  regions = regions |>
    mutate(label = LETTERS[seq_len(nrow(regions))])
  
  buoys = buoys |>
    mutate(label = substring(id, 1,1))
  
  ghcn = ghcn |>
    mutate(label = LETTERS[seq_len(nrow(ghcn))])
  
  usgs = usgs |>
    mutate(label = seq_len(nrow(usgs)))
  
  gg = ggOceanMaps::basemap(limits = bb, 
                       bathymetry =  TRUE,
                       bathy.style = "raster_user_blues",
                       legends = FALSE) +
    geom_sf(data = regions, 
            mapping = aes(color = name), 
            fill = NA, 
            linewidth = .8,
            show.legend = FALSE) +
    geom_sf_text(data = regions, 
                  aes(label = name))
  
  if (!is.null(buoys)) {
    gg = gg + 
      geom_sf(data = buoys,
              color = "black", pch = 16, size = 6) +
      geom_sf_text(data = buoys, 
                   aes(label = label), color = 'white')
  }
  
  if (!is.null(ghcn)){
    gg = gg + 
      geom_sf(data = ghcn,
              color = "black", pch = 15, size = 6) +
      geom_sf_text(data = ghcn, 
                   aes(label = label), color = 'white')
  }
  
  if (!is.null(usgs)){
    gg = gg + 

      geom_sf(data = usgs,
              color = "black", pch = 17, size = 6) +
      geom_sf_text(data = usgs, 
                   aes(label = label), color = 'white')
  }
  
  gg$labels$x = NULL
  gg$labels$y = NULL
 
  gg 
}