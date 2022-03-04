# Housekeeping ----------------------------------------------------------------

library(dplyr)
library(leafem)
library(leaflet)
library(janitor)
library(jsonlite)
library(raster)
library(readr)
library(rvest)

"
Data file formats:
  1) Shapefile
  2) Raster
  3) GeoTiff
  4) JSON
"

# Tidy data -------------------------------------------------------------------

# Ski resorts ---------------------------------------------

resorts <- '
  https://en.wikipedia.org/wiki/
  Comparison_of_North_American_ski_resorts
'
resorts <- gsub('[[:space:]]|[\r\n]', '', resorts)

# Extract relevant table on page
resorts <- read_html(resorts) %>% 
  html_nodes('table') %>%
  `[[`(5)

# Import HTML table as dataframe
resorts_df <- html_table(resorts, trim = T, fill = T)
resorts_df <- clean_names(resorts_df)

# Break HTML table into rows
resorts_rows <- resorts %>% 
  html_nodes('tbody') %>%
  html_nodes('tr')

# Extract node links and titles
extract_attrs <- function(x) html_attrs(html_nodes(x, 'a'))

resorts_links <- lapply(resorts_rows, extract_attrs) 
resorts_links <- lapply(resorts_links, function(x) x[[1]])
resorts_links <- lapply(resorts_links, function(x) data.frame(t(x)))
resorts_links <- plyr::rbind.fill(resorts_links)

glimpse(resorts_links)

# Rows: 465
# Columns: 4
# $ href  <chr> "/wiki/Lift_ticket", "/wiki/Ski_Bromont", "/wiki/Apex_Mountain_Resort", "/wiki/Canyon_Sk…
# $ title <chr> "Lift ticket", "Ski Bromont", "Apex Mountain Resort", "Canyon Ski Area", "Fernie Alpine …
# $ class <chr> NA, NA, NA, NA, NA, NA, NA, NA, "new", "mw-redirect", NA, NA, "new", NA, NA, "mw-redirec…
# $ rel   <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …

resorts_links <- resorts_links %>%
  filter(
    is.na(rel) # Strip out nofollow links
  ) %>%
  mutate(
    href = gsub('/wiki/', '', href)
  )

# Some page names have redirects
redirects <- filter(resorts_links, class == 'mw-redirect')
redirects <- arrange(redirects, title)

kable(redirects, 'simple', align = 'c')

#                  href                                     title                       class       rel 
# ---------------------------------------  ---------------------------------------  -------------  -----
#          Belleayre_Ski_Center                     Belleayre Ski Center             mw-redirect    NA
#            Bousquet_Ski_Area                        Bousquet Ski Area              mw-redirect    NA
#             Boyne_Highlands                          Boyne Highlands               mw-redirect    NA
#             Boyne_Mountain                           Boyne Mountain                mw-redirect    NA
#     Buckhorn_Ski_and_Snowboard_Club          Buckhorn Ski and Snowboard Club       mw-redirect    NA
#            Cypress_Mountain                         Cypress Mountain               mw-redirect    NA
#          Great_Divide_Ski_Area                    Great Divide Ski Area            mw-redirect    NA
#              June_Mountain                            June Mountain                mw-redirect    NA
#       Lake_Louise_Mountain_Resort              Lake Louise Mountain Resort         mw-redirect    NA
#  Lee_Canyon_(Ski_and_Snowboard_Resort)    Lee Canyon (Ski and Snowboard Resort)    mw-redirect    NA
#                Mt_Baker                                 Mt Baker                   mw-redirect    NA
#               Mt._Norquay                              Mt. Norquay                 mw-redirect    NA
#           Mt._Rose_Ski_Tahoe                       Mt. Rose Ski Tahoe              mw-redirect    NA
#        Panorama_Mountain_Village                Panorama Mountain Village          mw-redirect    NA
#            Saddleback_Maine                         Saddleback Maine               mw-redirect    NA
#       Schweitzer_Mountain_Resort               Schweitzer Mountain Resort          mw-redirect    NA
#             Shames_Mountain                          Shames Mountain               mw-redirect    NA
#              Sierra_Summit                            Sierra Summit                mw-redirect    NA
#           Smuggler%27s_Notch                        Smuggler's Notch               mw-redirect    NA
#             Summit_Ski_Area                          Summit Ski Area               mw-redirect    NA
#            Sunshine_Village                         Sunshine Village               mw-redirect    NA
#     Tahoe_Donner_Downhill_Ski_Area           Tahoe Donner Downhill Ski Area        mw-redirect    NA
#             Taos_Ski_Valley                          Taos Ski Valley               mw-redirect    NA
#           Wolf_Creek_Ski_Area                      Wolf Creek Ski Area             mw-redirect    NA

"
Loop through redirects to get proper page title
"

getDeets <- function(x) {
  
  print(x)

  tryCatch({
    
    api_string <- '
      https://en.wikipedia.org/w/api.php?action=query&format=json
      &prop=extracts&titles={x}&redirects=1&exintro=1
    '
    api_string <- gsub('[[:space:]]|[\r\n]', '', api_string)

    api_call <- GET(glue(api_string))
    output <- fromJSON(rawToChar(api_call$content))
    
    return(output)
    
  }, error = function(e) NA
  
  )

}

resorts_links[1:10, ] %>% 
  purrr::pmap(getDeets(href))


resorts_calls <- lapply(resorts_links$href[1:10], getDeets)
resorts_calls <- lapply(resorts_calls, function(x) data.frame(x$query$pages[[1]]))
resorts_calls <- lapply(resorts_calls, function(x) mutate(as_tibble(x), extract = list(extract)))
resorts_calls <- plyr::rbind.fill(resorts_calls)
resorts_calls <- as_tibble(resorts_calls)











links[[1]] <- NULL
links <- lapply(links, function(x) link = gsub('/wiki/', '', x[1]))

names(links) <- df$resort_name_and_website

links <- plyr::ldply(links, data.frame)

coords <- function(x) {
  print(x)
  tryCatch({
    poo <- page_content(
      language = 'en',
      project = 'wikipedia',
      page_name = trimws(x)
    )
    
    poo <- poo$parse$text$`*`
    
    loo <- read_html(poo) %>% 
      html_nodes("span.geo-dec") %>% 
      html_text() %>%
      unique()
    loo <- loo[1]
    loo <- strsplit(loo, ' ')
    co <- as.numeric(gsub('[^0-9.-]', '', loo[[1]]))
    sign <- gsub('[^A-Z]', '', loo[[1]])
    sign <- ifelse(sign %in% c('S', 'W'), -1, 1)
    loo <- co * sign
    loo <- paste0(loo, collapse = ', ')
    
    return(loo)
    
  }, error = function(e) return(NA)
  )
}

xy <- lapply(links$X..i.., coords)

links$cord <- xy

df <- left_join(df, links, by = c('resort_name_and_website' = '.id'))

leaflet() %>%
  addProviderTiles(providers$Stamen.Toner) %>%
    addGeotiff(
      file = '/Users/edwardgivens/Downloads/tri_5KMmd_GMTEDmd.tif'
      , opacity = 0.9
      , colorOptions = colorOptions(
        palette = hcl.colors(256, palette = "viridis")
        , na.color = "transparent"
      )
    ) %>%
  addCircleMarkers(
    data = df,
    lat = ~as.numeric(gsub(',.*', '', cord)),
    lng = ~as.numeric(gsub('.*,', '', cord))
  )



"
Some combination of terrain roughness, diversity, precipitation and population
"

# Terrain ---------------------------------------------------------------------

"
http://www.earthenv.org/topography
https://www.nature.com/articles/sdata201840
"

states <- states(cb = TRUE)

# Ruggedness Geotiff -> Raster -> Shapefile
r <- raster('/Users/edwardgivens/Downloads/tri_50KMmd_GMTEDmd.tif')
rs <- rasterToPolygons(r, fun = function(x) x > 40)

leaflet(rs) %>% addTiles()

# Population area
hs <- raster('/Users/edwardgivens/Git/ski-town/data/usa_ppp_2020_1km_Aggregated_UNadj.tif')
hs <- rasterToPolygons(hs, fun = function(x) x >= 1000)
hs <- st_as_sf(hs)
hs <- sf::st_buffer(hs, dist = .1)
# st_union(hs, by_feature = T) -> bhs
# st_intersection(hs) -> bhs

sti <- st_intersects(hs)
stip <- sti[lengths(sti) >= 10] # ID blocks that >= 10 intersections
nhs <- hs[unique(unlist(stip)), ]

bhs <- st_union(nhs)
centers <- st_centroid(st_cast(bhs, 'POLYGON'))
centers <- data.frame(st_coordinates(centers))
centers$coor <- paste0(centers$Y, ',', centers$X)

readable <- function(x) {
  string <- 'https://api.tomtom.com/search/2/reverseGeocode/{x}.json?key=&entityType=Municipality&radius=100000'
  uh <- GET(glue(string))
  addy <- fromJSON(rawToChar(uh$content))$addresses$address$freeformAddress
  Sys.sleep(5)
  print(x)
  return(list(x, addy))

}

lapply(centers$coor[1:5], readable)

fromJSON(rawToChar(uh$content))$addresses$address$freeformAddress

# bhs <- gBuffer(hs, width = .1)

leaflet(hs) %>% addProviderTiles(providers$Stamen.Toner) %>% addPolygons()
leaflet(bhs) %>% addProviderTiles(providers$Stamen.Toner) %>% addPolygons()
leaflet(rs) %>% addProviderTiles(providers$Stamen.Toner) %>% addPolygons()

colss <- RColorBrewer::brewer.pal(n = 10, name = 'Spectral')

leaflet() %>%
  addProviderTiles(providers$Stamen.Toner) %>%
  addPolygons(data = bhs, color = 'blue') %>%
  addCircles(data = data.frame(centers), lng = ~X, lat = ~Y) %>%
  addPolygons(data = rs, color = 'red') %>%
  addRasterImage(wx, opacity = .4, colors = colss)

# TomTom

readable <- function(x) {
  string <- 'https://api.tomtom.com/search/2/reverseGeocode/{x}.json?key=&entityType=Municipality&radius=100000'
  uh <- GET(glue(string))
  addy <- fromJSON(rawToChar(uh$content))$addresses$address$freeformAddress
  return(data.frame(x, addy))
}

lapply(centers$coor[1:5], readable)


w <- raster('/Users/edwardgivens/Downloads/MAT.tif')
ws <- rasterToPolygons(w, function(x) x > 40)


"
https://prism.oregonstate.edu/normals/
"

wx <- raster('/Users/edwardgivens/Downloads/PRISM_soltotal_30yr_normal_4kmM3_annual_bil/PRISM_soltotal_30yr_normal_4kmM3_annual_bil.bil')
wx <- rasterToPolygons(wx, function(x) x > 20)
wx <- st_as_sf(wx)
wx <- st_union(wx)
leaflet(wx) %>% addProviderTiles(providers$Stamen.Toner) %>% addPolygons()

# Wind -----------------------------------

'/Users/edwardgivens/Git/ski-town/data/wtk_conus_10m_mean_masked.tif'

wind <- raster('/Users/edwardgivens/Downloads/us-wind-data/wtk_conus_200m_mean_masked.tif')


# Tidy data -------------------------------------------------------------------

# Population areas ----------------------------------------


# Roughness -----------------------------------------------


# Ski areas -----------------------------------------------


# Fishing rivers ------------------------------------------

# National Parks ------------------------------------------


# Weather -------------------------------------------------

# Sunshine ----------------------------

# Temps -------------------------------

# Precipitation -----------------------

# Wind speed --------------------------

"
https://maps.nrel.gov/wind-prospector/
"

wind <- st_read('/Users/edwardgivens/Downloads/wind_prospector-wtk_40m_windspeed_2012.json')


empty <- raster(wind, res = 1)
wind <- fasterize(wind, empty)

# Hazards -------------------------------------------------

# Earthquakes -------------------------

# Hurricanes --------------------------


# Tornadoes ---------------------------



# Politics ------------------------------------------------



# Comforts ------------------------------------------------

"
Costco, Trader Joe's, Whole Foods, Chipotle
"



hs[hs[] < 100 ] <- NA
hs <- as.data.frame(hs, xy = T)
hs <- filter(hs, !is.na(layer))

weights <- with(hs, kde2d.weighted(x, y, w = layer, n = 100))
df <- data.frame(expand.grid(x = weights$x, y = weights$y), z = as.vector(weights$z))
df$ec <- ecdf(df$z)(df$z)
df <- filter(df, ec > .95)

leaflet(hs) %>% addProviderTiles(providers$Stamen.Toner) %>% addCircles(data = df, lng = ~x, lat = ~y)


p1 <- ggplot() +
  geom_contour(
    data = dfdens,
    aes(
      x = x,
      y = y,
      z = z
    )
  )

df <- ggplot_build(p1)$data[[1]]
dff <- filter(df, piece == 2 | piece == 3 | piece == 4)

ggplot() +
  geom_point(
    data = dff,
    aes(
      x = x,
      y = y,
      color = factor(piece)
    )
  )

hs <- rasterToPolygons(hs, fun = function(x) x > 5000)

poi <- lapply(hs@polygons, function(x) data.frame(t(c(x@ID, x@labpt))))
poi <- plyr::ldply(poi, data.frame)

poi$X2 <- as.numeric(poi$X2)
poi$X3 <- as.numeric(poi$X3)

kk <- MASS::kde2d(poi$X2,poi$X3)
dx <- diff(kk$x[1:2])
dy <- diff(kk$y[1:2])
sz <- sort(kk$z)
c1 <- cumsum(sz) * dx * dy
approx(c1, sz, xout = 1 - prob)$y

coordinates(poi) <- ~X2 + X3
poi <- gBuffer(poi, width = .5, byid = T)

leaflet(hs) %>% addProviderTiles(providers$Stamen.Toner) %>% addPolygons(data = poi)

poid <- st_as_sf(poi, coords = c("X2", "X3"), crs = 4326) %>% 
  st_transform(3035)

poid <- st_buffer(poid, dist = 100)


poid <- as(poid, 'Spatial')

pois <- group_split(poi, X1)
lapply(pois, function(x) sf::st_as_sf(select(x, X2, X1), coords = c('X2', 'X1'), crs = 4326))

leaflet(hs) %>% addProviderTiles(providers$Stamen.Toner) %>% addPolygons()

leaflet(r) %>% addProviderTiles(providers$Stamen.Toner) %>% addPolygons(fillColor = ~colorQuantile("YlOrRd", tri_50KMmd_GMTEDmd)(tri_50KMmd_GMTEDmd), fillOpacity = .9)
  

r <- as.data.frame(ruggedness, xy = T)
pnts_sf <- st_as_sf(ruff, coords = c('x', 'y'), crs = st_crs(states))

pnts <- pnts_sf %>% 
  mutate(
    intersection = as.integer(st_intersects(geometry, states)), 
    area = if_else(is.na(intersection), '', states$STUSPS[intersection])
)
pnts <- pnts[!is.na(pnts$intersection), ]
pnts <- rename(pnts, tri = tri_5KMmd_GMTEDmd)

zoom <- filter(pnts, area %in% c('CA', 'IL', 'CO', 'WA'))

ggplot() +
  geom_jitter(
    data = zoom,
    aes(
      x = area,
      y = tri,
      color = area
    )
  )
  

leaflet() %>%
  addProviderTiles(providers$Stamen.Toner) %>%
  addPolygons(
    data = states,
    fillColor = "white",
    color = "black",
    weight = 0.5) %>%
  addGeotiff(
    file = '/Users/edwardgivens/Downloads/tri_5KMmd_GMTEDmd.tif',
    opacity = 0.9,
    colorOptions = colorOptions(
      palette = hcl.colors(256, palette = "viridis"),
      na.color = "transparent"
    )
  ) %>%
  addRasterImage(
    x = hs,
    project = F,
    colors = pal
  )
  #   ) %>%
  # addCircleMarkers(
  #   data = resorts,
  #   lat = 37.9,
  #   lng = -107.8
  # )

hs <- raster('/Users/edwardgivens/Git/ski-town/data/usa_ppp_2020_1km_Aggregated_UNadj.tif')

hs <- as.data.frame(hs, xy = T)
hs <- rename(hs, pop_density = usa_ppp_2020_1km_Aggregated_UNadj)
hs <- filter(hs, !is.na(pop_density) & pop_density > 1000)

hmm <- cclust(hs[, 1:2], k=10, save.data=TRUE,weights =hs$pop_density,method="hardcl")

hs <- rasterFromXYZ(hs)
newproj <- "+proj=longlat +datum=WGS84"
crs(hs) <- newproj



# Population centers --------------------------------------









glimpse(resorts)

# Rows: 464
# Columns: 12
# $ resort_name_and_website                    <chr> "Ski Bromont", "Apex Mountain Resort", "Ca…
# $ nearest_city                               <chr> "Bromont", "Penticton", "Red Deer", "Ferni…
# $ state_province                             <chr> "Quebec", "British Columbia", "Alberta", "…
# $ peak_elevation_ft                          <chr> "1,854", "7,197", "2,950", "7,000", "1,791…
# $ base_elevation_ft                          <chr> "590", "5,197", "2,412", "3,450", "33", "4…
# $ vertical_drop_ft                           <chr> "1,264", "2,000", "538", "3,550", "1,759",…
# $ skiable_acreage                            <chr> "450", "1,112", "80", "2,500", "230", "55"…
# $ total_trails                               <int> 141, 79, 23, 142, 40, 27, 20, 15, 28, 94, …
# $ total_lifts                                <int> 9, 4, 6, 10, 5, 2, 3, 3, 3, 6, 6, 5, 5, 11…
# $ avg_annual_snowfall_in                     <chr> "190", "236", "45", "360", "192", "100", "…
# $ adult_weekend_lift_ticket_window_price_usd <chr> "$54", "$65", "$35", "$90", "$34", "$38", …
# $ date_statistics_updated                    <chr> "December 1, 2019[1]", "November 30, 2019[…

mutate_cols <- c(
  'base_elevation_ft', 
  'vertical_drop_ft', 
  'skiable_acreage', 
  'avg_annual_snowfall_in',
  'total_trails',
  'total_lifts'
)

resorts <- resorts %>%
  arrange(
    state_province,
    desc(vertical_drop_ft)
  ) %>%
  mutate_all(
    function(x) gsub('\\[.*\\]', '', x)
  ) %>%
  mutate_at(
    mutate_cols,
    function(x) parse_number(x)
  )

states <- resorts %>%
  group_by(
    state_province
  ) %>%
  summarize(
    n = n(),
    base = median(base_elevation_ft, na.rm = T),
    drop = median(vertical_drop_ft, na.rm = T),
    snowfall = median(avg_annual_snowfall_in, na.rm = T),
    .groups = 'drop'
  ) %>%
  arrange(
    -n
  ) %>%
  mutate(
    across(n:snowfall, ~ scale(.x, center = F)[, 1])
  ) %>%
  pivot_longer(
    cols = n:snowfall
  )

ggplot() +
  geom_bar(
    data = states,
    aes(
      x = name,
      y = value,
      fill = name
    ),
    stat = 'identity',
    width = 1
  ) +
  facet_wrap(~state_province) +
  coord_polar()


"
https://ghsl.jrc.ec.europa.eu/download.php?ds=bu
https://urban-tep.eu/#!pages/dataservices
http://www.earthenv.org/topography
"
