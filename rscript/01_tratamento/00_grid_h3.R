#' A base espacial de análise utilizada na dissertação foi a biblioteca de hexágonos
#' desenvolvida pela Uber. O código foi adaptado em 01/2023 para estar adequado com a
#' nova nomenclatura da biblioteca utilizada `h3jsr`

# Libraries ----
library(tidyverse)
library(h3jsr)

grid_resolution <- 9

# Files ----
msp <- sf::st_read('data/shp/MSP.shp') %>% 
    sf::st_transform(crs = 4326) # WGS84 degrees

# H3 ----

# get the ids
# get the unique h3 ids of the hexagons intersecting your polygon at a given resolution
hex <- polygon_to_cells(msp, res = grid_resolution, simple = FALSE)

# pass the h3 ids to return the hexagonal grid
hex <- unlist(hex$h3_addresses) %>% 
  cell_to_polygon(simple = FALSE)

grid <- hex %>% 
    mutate(id = h3_address) %>% 
    select(id)

# Saving ----
# as shapefile

sf::write_sf(grid, paste0('data/shp/grid_hex_MSP_', grid_resolution, '.shp'))

# Points coord ----

points <- grid %>% 
  rowwise() %>% 
  mutate(coords = paste0(sf::st_coordinates(cell_to_point(id)), collapse=',')) %>% 
  separate(col = coords, into = c('X', 'Y'), sep=',') %>% 
  sf::st_drop_geometry()

write_csv(points, paste0('data/shp/grid_hex_MSP_', grid_resolution, '.csv'))