###############################################################################
# Title: Where Should I Live?                                                 #
# Date: 2022-02-10                                                            #
# Description: Determine where in CONUS I should live given what's important  #
# to me                                                                       #
###############################################################################

# Housekeeping ----------------------------------------------------------------

library(dplyr)
library(glue)
library(httr)
library(jsonlite)
library(leaflet)
library(raster)
library(sf)
library(tigris)

setwd('/Users/edwardgivens/Git/ski-town/')

# Tidy data -------------------------------------------------------------------

# CONUS shapefile -----------------------------------------

states <- states(cb = T)

"
Head west, young man
"

west <- c('CO')
west <- states[states$STUSPS %in% west, ]
west <- st_union(west)

# Population centers --------------------------------------

"
This one's gnarly. Let's take it one step at a time
"

# Prevent error
sf::sf_use_s2(FALSE)

"
https://data.humdata.org/dataset/worldpop-population-counts-for-united-states-of-america
"

pop <- raster('./data/usa_ppp_2020_1km_Aggregated_UNadj.tif')

# Filter for square km grids that have >= 1000 people
pop <- rasterToPolygons(pop, fun = function(x) x >= 1000)

# Convert to simple features object
pop <- st_as_sf(pop)

# Set CRS to the same one as the state polygons; find overlap
st_crs(pop) <- crs(west) 
pop <- st_intersection(pop, west)

# Find significantly overlapping polygons within subset
ipop <- st_intersects(pop)
ipop <- ipop[lengths(ipop) >= 2] # Return polygons with >= 8 overlaps
pop <- pop[unique(unlist(ipop)), ]

# Join together neighboring, overlapping polygons
pop <- st_union(pop)

# Expand polygons to fill in gaps (say within metro area)
pop <- pop %>%
    st_buffer(
    dist = .1 # .1 arc degrees ~ 10km
  )

# Trim coastal areas
pop <- st_intersection(pop, west)

# Cast to individual polygons
pops <- st_cast(pop, 'POLYGON')
pops <- data.frame(pops)
pops$area <- st_area(pops[[1]])
pops <- pops[as.numeric(pops$area) >= (10000 * 10000), ]
# pops <- select(pops, -area)
pops <- st_as_sf(pops)

centers <- st_centroid(pops)

leaflet(pops) %>% 
  addTiles() %>% 
  addPolygons() %>%
  addCircles(data = centers, color = 'red')

centers <- data.frame(st_coordinates(centers))
centers$coords <- with(centers, paste0(Y, ', ', X))

readable <- function(coords, api, key) {
  
  Sys.sleep(5)

  api_call <- '{api}{coords}.json?key={key}&entityType=Municipality'
  result <- GET(glue(api_call))
  result <- fromJSON(rawToChar(result$content))
  addy <- result$addresses$address$freeformAddress
  print(addy)
  
  df <- data.frame(coords = coords, location = addy)
  return(df)

}

api <- 'https://api.tomtom.com/search/2/reverseGeocode/'

places <- lapply(centers$coords[sample(nrow(centers), 64, F)], readable, api = api, key = key)
places <- do.call(rbind, places)
places <- left_join(centers, places, by = 'coords')
places <- filter(places, !is.na(location))

sfplaces <- st_as_sf(places, coords = c('X', 'Y'), crs = 4326)
sfplaces <- st_transform(sfplaces, crs = 7801)
sfplaces50 <- st_buffer(sfplaces, dist = 80467.2, nQuadSegs = 200)
sfplaces100 <- st_buffer(sfplaces, dist = 80467.2 * 2, nQuadSegs = 200)
sfplaces300 <- st_buffer(sfplaces, dist = 80467.2 * 6, nQuadSegs = 200)
sfplaces50 <- st_transform(sfplaces50, crs = 4326)
sfplaces100 <- st_transform(sfplaces100, crs = 4326)
sfplaces300 <- st_transform(sfplaces300, crs = 4326)

st_crs(sfplaces50) <- crs(west) 
sfplaces50 <- st_intersection(sfplaces50, west)

"
Reimport populationd density Raster
"

pop <- raster('./data/usa_ppp_2020_1km_Aggregated_UNadj.tif')

pop <- aggregate(pop, fact = 50, fun = sum)

# Filter for square km grids that have > 0 people
pop <- rasterToPolygons(pop, fun = function(x) !is.na(x) & x > 0)
pop <- st_as_sf(pop)
st_crs(pop) <- crs(sfplaces50) 
pop <- st_intersection(pop, sfplaces50)

leaflet(pop) %>% 
  addTiles() %>% 
  addPolygons(
    fillColor = ~colorQuantile(usa_ppp_2020_1km_Aggregated_UNadj, palette = 'Blues')(usa_ppp_2020_1km_Aggregated_UNadj), 
    fillOpacity = .9
  )

gop <- pop %>% 
  group_by(location) %>% 
  summarize(total_pop = sum(usa_ppp_2020_1km_Aggregated_UNadj)) %>%
  arrange(-total_pop)

# Precipitation -------------------------------------------



# Ruggedness ----------------------------------------------

"
Import ruggedness Rasters, convert to Polygons for intersection
calculations, then convert back to Rasters
"

rug <- raster('./data/tri_50KMmd_GMTEDmd.tif') # Read in GeoTiff
rug <- rasterToPolygons(rug) # Convert to polygons
rug <- st_as_sf(rug) # Convert to sf object
st_crs(rug) <- crs(tx) # Set CRS to the same one as the state polygons
rug <- st_intersection(rug, tx) # Find the overlap

leaflet(rug) %>%
  addTiles() %>%
  addPolygons(
    fillColor = ~colorQuantile('viridis', n = 8, tri_50KMmd_GMTEDmd)(tri_50KMmd_GMTEDmd),
    color = NA
  )

# Sunshine ------------------------------------------------

sun <- raster('./data/PRISM_soltotal_30yr_normal_4kmM3_annual_bil/PRISM_soltotal_30yr_normal_4kmM3_annual_bil.bil')
sun <- aggregate(sun, 10)
sun <- rasterToPolygons(sun)

sun <- st_as_sf(sun)
st_crs(sun) <- crs(tx) 
sun <- st_intersection(sun, tx)

leaflet(sun) %>%
  addTiles() %>%
  addPolygons(
    fillColor = ~colorQuantile('YlOrRd', n = 8, PRISM_soltotal_30yr_normal_4kmM3_annual_bil)(PRISM_soltotal_30yr_normal_4kmM3_annual_bil),
    color = NA
  )

# Temps ---------------------------------------------------

temps <- raster('./data/PRISM_tmean_30yr_normal_4kmM3_annual_bil/PRISM_tmean_30yr_normal_4kmM3_annual_bil.bil')
temps <- aggregate(temps, 10)
temps <- rasterToPolygons(temps)

temps <- st_as_sf(temps)
st_crs(temps) <- crs(tx) 
temps <- st_intersection(temps, tx)

colss <- colorQuantile('RdYlBu', domain = temps$PRISM_tmean_30yr_normal_4kmM3_annual_bil, n = 8, reverse = T)

leaflet(temps) %>%
  addTiles() %>%
  addPolygons(
    fillColor = ~colss(PRISM_tmean_30yr_normal_4kmM3_annual_bil),
    color = NA
  ) %>%
  addPolygons(
    data = states
  )

# Wind ----------------------------------------------------

wind <- raster('./data/wtk_conus_200m_mean_masked.tif')
wind <- flip(wind, direction = 'y')
wind <- projectRaster(wind, rug)

wind <- rasterToPolygons(wind) # Convert to polygons
wind <- st_as_sf(wind)
st_crs(wind) <- crs(tx) 
wind <- st_intersection(wind, tx) # Find the overlap

leaflet(wind) %>%
  addTiles() %>%
  addPolygons(
    fillColor = ~colorQuantile('Blues', n = 8, wtk_conus_200m_mean_masked)(wtk_conus_200m_mean_masked),
    color = NA
  )

# Natural disasters ---------------------------------------

# Earthquakes -------------------------

# Fire --------------------------------

# Flooding ----------------------------



# National Parks ------------------------------------------

"
https://public-nps.opendata.arcgis.com/
"


# Tax Burden ----------------------------------------------



# Target test ---------------------------------------------



# Map -----------------------------------------------------

leaflet() %>%
  addProviderTiles(
    providers$Stamen.Toner
  ) %>%
  addRasterImage(
    rug,
    opacity = .3,
    group = 'Ruggedness'
  ) %>%
  addRasterImage(
    wind,
    opacity = .3,
    group = 'Wind'
  ) %>%
  addLayersControl(
    overlayGroups = c('Ruggedness', 'Wind'),
    options = layersControlOptions(collapsed = F)
  )