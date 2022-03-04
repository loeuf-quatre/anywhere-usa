###############################################################################
# Title: Anywhere USA                                                         #
# Date: 02 March 2022                                                         #
# Description:                                                                #
###############################################################################

# Housekeeping ----------------------------------------------------------------

library(dplyr) # Data wrangling
library(glue) # TomTom API call
library(httr) # TomTom API call
library(jsonlite) # TomTom API call
library(keyring) # TomTom API call
library(leaflet) # Mapping
library(mapview) # Export mapping
library(raster) # Process raster images
library(sf) # Manipulate shapefiles
library(tigris) # Geoboundaries

setwd('/Users/edwardgivens/Git/anywhere-usa/')

# Tidy data -------------------------------------------------------------------

# CONUS shapefile -----------------------------------------

states <- states(cb = T)

non_conus <- c(
  'AK', # Alaska
  'AS', # American Samoa
  'GU', # Guam
  'HI', # Hawaii
  'PR', # Puerto Rico
  'MP', # Mariana Islands
  'VI' # US Virgin Islands
)

conus <- states[!states$STUSPS %in% non_conus, ]
conus <- st_union(conus)

conus_center <- st_coordinates(st_centroid(conus))

m1 <- leaflet(conus) %>%
  setView(
    lng = conus_center[1],
    lat = conus_center[2],
    zoom = 4
  ) %>%
  addProviderTiles(
    providers$Stamen.Toner
  ) %>%
  addPolygons(
    color = 'red',
    opacity = 1,
    fillColor = 'red',
    fillOpacity = .1,
    weight = 2
  )

mapshot(m1, file = './output/conus.png')

# Population centers --------------------------------------

"
This one's gnarly. Let's take it one step at a time
"

# Prevent error
sf::sf_use_s2(FALSE)

"
https://data.humdata.org/dataset/worldpop-population-counts-for-united-states-of-america
"

pop_raster <- raster('./data/usa_ppp_2020_1km_Aggregated_UNadj.tif')

# Filter for square km grids that have >= 1000 people (~suburban density)
pop <- rasterToPolygons(pop_raster, fun = function(x) x >= 1000)

# Convert to simple features object
pop <- st_as_sf(pop)

# Set state polygons CRS to same as population density; find overlap
conus <- st_transform(conus, crs = crs(pop))
pop <- st_intersection(pop, conus)

# Zoom into Provo, Utah
m2 <- leaflet(pop) %>%
  setView(
    lng = -111.66,
    lat = 40.23,
    zoom = 11
  ) %>%
  addProviderTiles(
    providers$Stamen.Toner
  ) %>%
  addPolygons(
    color = 'red',
    opacity = 1,
    fillColor = 'red',
    fillOpacity = .1,
    weight = 1
  )

mapshot(m2, file = './output/provo.png')

# Expand polygons to fill in gaps (e.g. make contiguous metro areas)
pop <- pop %>%
  st_transform(
    crs = 7801
  ) %>%
  st_buffer(
    dist = 8046.7 # Expand 5 miles in each direction
  ) %>%
  st_transform(
    crs = 4326
  ) %>%
  rowwise() %>% 
  mutate(
    geometry = st_make_grid(geometry, n = 1)
  ) %>%
  st_union() %>%
  st_intersection(
    conus
  )

# Zoom in on Utah and surrounding states' contiguous areas
utah <- st_transform(utah, crs = crs(pop))
utah_pop <- st_intersection(pop, west)

m3 <- leaflet(utah_pop) %>%
  setView(
    lng = -111.59,
    lat = 39.45,
    zoom = 7
  ) %>%
  addProviderTiles(
    providers$Stamen.Toner
  ) %>%
  addPolygons(
    color = 'red',
    opacity = 1,
    fillColor = 'red',
    fillOpacity = .1,
    weight = 1
  )

mapshot(m3, file = './output/utah.png')

"
We'll need to convert our MULTIPOLYGON object into distinct POLYGON objects
to find each one's centroid
"

pops <- st_cast(pop, 'POLYGON')
pops <- pops %>% 
  data.frame() %>% 
  mutate(
    .id = row_number()
  ) %>% 
  st_as_sf()
centers <- st_centroid(pops)

# Plot with centroids
m4 <- leaflet(pops) %>%
  addProviderTiles(
    providers$Stamen.Toner
  ) %>%
  addPolygons(
    color = 'red',
    opacity = 1,
    fillColor = 'red',
    fillOpacity = .1,
    weight = 1
  ) %>%
  addCircles(
    data = centers, 
    color = 'blue'
  )

"
Create 20km radius around each polygon centroid
"

centers <- st_transform(centers, crs = 7801)
buffer25 <- centers %>%
  st_buffer(
      dist = 32186.88
  ) %>%
  st_transform(
      crs = 4326
  )

m5 <- leaflet(pops) %>%
  addProviderTiles(
    providers$Stamen.Toner
  ) %>%
  addPolygons(
    color = 'red',
    opacity = 1,
    fillColor = 'red',
    fillOpacity = .1,
    weight = 1
  ) %>%
  addPolygons(
    data = buffer25, 
    color = 'blue'
  )

"
We'll loop through 25km buffered rows, summarizing population within buffer.
The more populous buffers will subsume the less populous buffers when there's
overlap
"

pop_agg <- aggregate(pop_raster, fact = 5, fun = sum)
pop_agg <- rasterToPolygons(pop_agg, fun = function(x) x > 0)
pop_agg <- st_as_sf(pop_agg)

buffer25pop <- buffer25 %>% 
  rowwise() %>% 
  st_as_sf() %>% 
  st_intersection(
    pop_clean
  ) %>%
  group_by(
    .id
  ) %>%
  summarize(
    total_pop = sum(usa_ppp_2020_1km_Aggregated_UNadj, na.rm = T),
    .groups = 'drop'
  ) %>%
  data.frame() %>%
  dplyr::select(
    -geometry
  )

buffer25pop <- left_join(buffer25, buffer25pop, by = '.id')
buffer25pop <- st_as_sf(buffer25pop)

pal <- colorQuantile(palette = 'Reds', buffer25pop$total_pop, n = 9)

m6 <- leaflet(buffer25pop) %>%
  addProviderTiles(
    providers$Stamen.Toner
  ) %>%
  addPolygons(
    data = pop,
    color = 'red',
    opacity = 1,
    fillColor = 'red',
    fillOpacity = .1,
    weight = 1
  ) %>%
  addPolygons(
    color = 'red',
    fillColor = ~pal(total_pop),
    fillOpacity = .7
  )

"
Loop through buffered, populated polygons to see which location polygons they
intersect with
"

delta <- 1

while(delta > 0) {
  
  buffer25pop_temp <- buffer25pop
  
  retain <- buffer25pop %>%
    rowwise() %>%
    st_as_sf() %>%
    st_intersection(
      pops
    ) %>%
    dplyr::select(
      .id,
      intersection.id = .id.1
    ) %>%
    arrange(
      .id,
      intersection.id
    ) %>%
    left_join(
      dplyr::select(data.frame(buffer25pop), .id, total_pop),
      by = c('intersection.id' = '.id')
    ) %>%
    group_by(
      .id
    ) %>%
    mutate(
      biggest_pop = max(total_pop, na.rm = T)
    ) %>%
    filter(
      total_pop == biggest_pop
    ) %>%
    pull(
      intersection.id
    ) %>%
    unique()
  
  buffer25pop <- filter(buffer25pop, .id %in% retain)
  
  delta <- nrow(buffer25pop_temp) - nrow(buffer25pop)
  
}

leaflet(filter(pops, .id %in% buffer25pop$.id)) %>%
  addTiles() %>%
  addPolygons()

"
Supply centroid coordinates to TomTom API to collect place names
"

centers <- filter(centers, .id %in% buffer25pop$.id)
centers <- st_transform(centers, crs = 4326)
centers <- data.frame(st_coordinates(centers))
centers$coords <- with(centers, paste0(Y, ', ', X))

getPlaces <- function(coords, api, key) {
  
  tryCatch({
    
    Sys.sleep(3)

    api_call <- '{api}{coords}.json?key={key}&entityType=Municipality'
    result <- GET(glue(api_call))
    result <- fromJSON(rawToChar(result$content))
    addy <- result$addresses$address$freeformAddress
    print(paste0(addy, ' ', Sys.time()))
    
    df <- data.frame(coords = coords, location = addy)
    return(df)
    
    }, error = function(e) return(NA)
  )
  
}

api <- 'https://api.tomtom.com/search/2/reverseGeocode/'

places <- lapply(centers$coords, getPlaces, api = api, key = key_get('TomTom'))
places <- do.call(rbind, places)

places <- left_join(centers, places, by = 'coords')
places <- filter(places, !is.na(location))
places$id <- seq_along(places$location)
places <- st_as_sf(places, coords = c('X', 'Y'), crs = 4326)

m5 <- leaflet(places) %>% 
  setView(
    lng = -111.59,
    lat = 39.45,
    zoom = 7
  ) %>%
  addProviderTiles(
    providers$Stamen.TonerBackground
  ) %>% 
  addLabelOnlyMarkers(
    data = places, 
    label = ~as.character(location), 
    labelOptions = labelOptions(
      noHide = T, 
      direction = 'top', 
      textOnly = F,
      style = list(
        'color' = 'black',
        'font-family' = 'sans-serif',
        'font-size' = '12px',
        'background-color' = '#FBF9E6',
        'border-color' = 'rgba(0, 0, 0, .5)'
      )
    )
  ) %>% 
  addCircles(
    color = 'black', 
    opacity = 1
  )

mapshot(m5, file = './output/utah-centers.png')

"
Create 10m (immediate vicinity), 50m (~1h drive), 150m (day trip) and
300m (road trip) perimeters around location coordindates
"

places <- st_transform(places, crs = 7801)

addBuffers <- function(buffer) {
  buffered_places <- places %>%
    st_buffer(
      dist = buffer
    ) %>%
    st_transform(
      crs = 4326
    ) %>%
    mutate(
      buffer = buffer
    )
  return(buffered_places)
}

buffers <- lapply(c(16093.44, 80467.2, 241401.6, 482803.2), addBuffers)
buffers <- do.call('rbind', buffers)

places <- st_transform(places, crs = 4326)

moab_buffers <- filter(buffers, location == 'Moab, UT')
moab_point <- filter(places, location == 'Moab, UT')

m6 <- leaflet(moab_buffers) %>%
  addProviderTiles(
    providers$Stamen.Toner
  ) %>%
  addPolygons(
    data = moab_buffers,
    color = ~colorQuantile(palette = 'Reds', domain = buffer)(buffer),
    fillColor = 'red',
    fillOpacity = 0,
    weight = 2
  ) %>%
  addLabelOnlyMarkers(
    data = moab_point, 
    label = ~as.character(location), 
    labelOptions = labelOptions(
      noHide = T, 
      direction = 'top', 
      textOnly = T,
      style = list(
        'color' = 'black',
        'font-family' = 'sans-serif',
        'font-size' = '12px'
      )
    )
  ) %>% 
  addCircles(
    data = moab_point,
    color = 'black', 
    opacity = 1
  )

mapshot(m6, file = './output/moab-buffers.png')

"
Reimport populationd density Raster
"

pop <- raster('./data/usa_ppp_2020_1km_Aggregated_UNadj.tif')

pop <- aggregate(pop, fact = 5, fun = sum)

# Filter for square km grids that have > 0 people
pop <- rasterToPolygons(pop, fun = function(x) !is.na(x) & x > 0)
pop <- st_as_sf(pop)

pop <- st_intersection(pop, west)

# popc <- st_intersection(pop, filter(buffers, location == 'Moab, UT' & buffer == 80467.2))
# 
# leaflet(moab) %>%
#   addTiles() %>%
#   addPolygons(
#     fillOpacity = 0
#   ) %>%
#   addPolygons(
#     data = popc,
#     fillColor = ~colorQuantile(
#       palette = 'Blues', 
#       usa_ppp_2020_1km_Aggregated_UNadj)(usa_ppp_2020_1km_Aggregated_UNadj),
#     fillOpacity = .8
#   )

# Join 50m radii geometries to population geometries
buffers50 <- filter(buffers, buffer == 80467.2)
buffered_pop <- st_join(buffers50, pop, by = 'geometry')

buffered_pop <- buffered_pop %>%
  group_by(
    location,
    id
  ) %>%
  summarize(
    population_50m = sum(usa_ppp_2020_1km_Aggregated_UNadj, na.rm = T),
    .groups = 'drop'
  ) %>%
  arrange(
    -population_50m
  )

buffered_pop

buffered_overlaps <- st_intersection(buffered_pop, places)
retain <- buffered_overlaps %>%
  group_by(
    coords
  ) %>%
  mutate(
    max_population_50m = max(population_50m)
  ) %>%
  filter(
    population_50m == max_population_50m
  ) %>%
  pull(id) %>%
  unique()

aces <- filter(places, id %in% retain)

m7 <- leaflet(aces) %>% 
  setView(
    lng = -111.59,
    lat = 39.45,
    zoom = 7
  ) %>%
  addProviderTiles(
    providers$Stamen.TonerBackground
  ) %>% 
  addLabelOnlyMarkers(
    data = aces, 
    label = ~as.character(location), 
    labelOptions = labelOptions(
      noHide = T, 
      direction = 'top', 
      textOnly = F,
      style = list(
        'color' = 'black',
        'font-family' = 'sans-serif',
        'font-size' = '12px',
        'background-color' = '#FBF9E6',
        'border-color' = 'rgba(0, 0, 0, .5)'
      )
    )
  ) %>% 
  addCircles(
    color = 'black', 
    opacity = 1
  )

mapshot(m7, file = './output/utah-centers-whittled.png')