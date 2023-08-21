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
                    ghcn = ghcn_lut() |>
                      dplyr::filter(mgrepl(analysis_stations('ghcn'), id, fixed = TRUE)),
                    buoys = NULL, #buoy_lut(form = 'sf'),
                    usgs = usgs_lut(form = 'sf') |>
                      dplyr::filter(mgrepl(analysis_stations('usgs'), site_no, fixed = TRUE)),
                    shapes = c(buoy = 16,  #circle
                               usgs = 17,  # triangle 
                               ghcn = 15), # square,
                    currents = read_currents(), 
                    bb = c(xmin = -72, ymin = 39, xmax = -62, ymax = 46)
                    ){
  
  
  if (FALSE){
    regions = read_regions()
    ghcn = ghcn_lut() |>
      dplyr::filter(mgrepl(analysis_stations('ghcn'), id, fixed = TRUE))
    buoys = NULL
    usgs = usgs_lut(form = 'sf') |>
      dplyr::filter(mgrepl(analysis_stations('usgs'), site_no, fixed = TRUE))
    currents = read_currents()
  }
  
  if (inherits(buoys, 'data.frame') && nrow(buoys) == 0) buoys = NULL
  if (inherits(ghcn, 'data.frame') && nrow(ghcn) == 0) ghcn = NULL
  if (inherits(usgs, 'data.frame') && nrow(usgs) == 0) usgs = NULL
  
  if (is.null(bb)){
    bb = sf::st_bbox(regions) |>
      as.vector()
    bb = bb[c(1,3,2,4)] + c(-.4, 0.2, -0.1, .2)
  } else {
    bb = bb[c("xmin", "xmax", "ymin", "ymax")]
  } 
  
  cat(str(bb), "\n")
  BB = sf::st_bbox(bb, crs = 4326)
  
  regions = regions |>
    mutate(label = LETTERS[seq_len(nrow(regions))])

  gg = ggOceanMaps::basemap(limits = bb, 
                       bathymetry =  TRUE,
                       bathy.style = "raster_user_blues",
                       legends = FALSE)
  
  if (!is.null(currents)){
    gg = gg + 
      geom_sf(data = sf::st_crop(currents, BB),
              
              show.legend = FALSE,
              arrow = grid::arrow(angle = 30, length = unit(0.25, "inches"),
                                  ends = "last", type = "open"),
              col = "grey", 
              linewidth = 4)
      
  }
  
  gg = gg +  
    geom_sf(data = regions, 
            mapping = aes(color = display_name), 
            fill = NA, 
            linewidth = .8,
            show.legend = FALSE) +
    geom_sf_text(data = regions, 
                  aes(label = display_name),
                 nudge_x = 0.1)
  
  if (!is.null(buoys)) {
    
    buoys = buoys |>
      mutate(label = substring(id, 1,1))
    
    gg = gg + 
      geom_sf(data = buoys,
              color = "black", pch = shapes[['buoys']], size = 6) +
      geom_sf_text(data = buoys, 
                   aes(label = label), color = 'white')
  }
  
  if (!is.null(ghcn)){
    
    ghcn = ghcn |>
      mutate(label = LETTERS[seq_len(nrow(ghcn))])
    
    gg = gg + 
      geom_sf(data = ghcn,
              color = "black", pch = shapes[['ghcn']], size = 6) +
      geom_sf_text(data = ghcn, 
                   aes(label = label), color = 'white')
  }
  
  if (!is.null(usgs)){
    
    
    usgs = usgs |>
      mutate(label = seq_len(nrow(usgs)))
    
    gg = gg + 
      geom_sf(data = usgs,
              color = "black", pch = shapes[['usgs']], size = 6) +
      geom_sf_text(data = usgs, 
                   aes(label = label), color = 'white')
  }
  
  gg$labels$x = NULL
  gg$labels$y = NULL
 
  gg 
}