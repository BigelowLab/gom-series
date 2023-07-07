#' Export datasets to a single wide format.
#' 
#' @param what char, vector of datasets to include 
#' @param by char the interval to aggregate over (one of 'year' or 'month')
export_all = function(what = c("all","sst", "chlor", "usgs", "ghcn", "indices")[1],
                      by = c("year", "month")[1]){
 
  if ("all" %in% what) what = c("sst", "chlor", "usgs", "ghcn", "indices")
  
  xx = sapply(what,
              function(w){
                switch(w,
                       "sst" = export_oisst())
              })
  
  
}