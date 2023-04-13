# Aggregating monthly oceanographic time series from the Gulf of Maine

Time span:
  
  - monthly
  - 1980-present

Region definition:
  - Polygons
    + Jordan Basin, Wilkinson Basin, Georges Basin, Georges Bank, EMCC, WMCC (SST, CHLOR_A)
To-do list
 - NAO, AMO -- these are done, in NOAAoscillations.R
 - SST
   + OISST
 - Chl-a
   + OBPG 4km
   + Copenicus (GLOBCOLOR)
 - Buoy (T, S, vel)
   + Multiple buoys?
 - River outflow
   + USGS
 - GNATS
 - DAYMET
   + precip
 - ...

### Data from buoys:

#### Buoys

+ Western Maine Shelf (B01)
+ Central Maine Shelf (E01)
+ Penobscot Bay (F01)
+ Eastern Maine Shelf (I01)
+ Jordan Basin (M01)
+ Northeast Channel (N01)

#### Variables:

Water temperature (all available depths)
Salinity (all available depths)
Current speed (all available depths)
Current direction (all available depths— but I’m not sure how to average this monthly?)
Air temperature (surface)
Chlorophyll (surface)
Wind speed (surface)
Wind direction (surface — but not sure how to average monthly)
Wind Gust

For the wind and current direction, if they provide monthly averages, that’s the best option, because then we don’t need to figure out how to do it.

There are others on the list that I’m interested in (dissolved oxygen, turbidity), but I don’t think there are many measurements for these buoys.
