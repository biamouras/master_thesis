#' Processes the grid and the centroids by the H3

# Libraries ----
library(tidyverse)
library(h3jsr)

# Files ----
msp <- sf::st_read('data/shp/MSP.shp') %>% 
    sf::st_transform(crs = 4326) # WGS84 degrees

# H3 ----

# get the ids
# get the unique h3 ids of the hexagons intersecting your polygon at a given resolution
hex_9 <- h3jsr::polyfill(msp, res = 9, simple = FALSE)

# pass the h3 ids to return the hexagonal grid
hex_9 <- unlist(hex_9$h3_polyfillers) %>% 
    h3jsr::h3_to_polygon(simple = FALSE)

grid <- hex_9 %>% 
    dplyr::mutate(id = h3_address) %>% 
    dplyr::select(id)

# Saving ----
# as shapefile

grid <- sf::as_Spatial(grid)
rgdal::writeOGR(grid, driver='ESRI Shapefile', dsn='data/shp/grid_hex_MSP.shp', layer='grid')

g <- sf::read_sf('data/shp/grid_hex_MSP.shp')

# Points coord ----
# After selecting only the cells that will show on the map
grid_clean <- sf::st_read('data/shp/grid_hex_MSP_clean.shp')

points <- grid_clean %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(coords = paste0(sf::st_coordinates(h3jsr::h3_to_point(id)), collapse=',')) %>% 
    tidyr::separate(col = coords, into = c('X', 'Y'), sep=',') %>% 
    sf::st_as_sf() %>% 
    sf::st_drop_geometry()

write.csv(points, 'otp/points/points_hex_clean.csv', row.names = F)
