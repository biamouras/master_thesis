# libraries ----
library(magrittr)
library(ggtern)

crsdegrees <- 4326

mcmv <- sf::st_read('data/shp/mcmv_sf.shp', quiet=T) %>% 
  sf::st_transform(crsdegrees) %>% 
  dplyr::filter(fase_mcmv != 3) %>% 
  dplyr::mutate(class = ifelse(faixa == 1, 'low', 'medium'),
                period = 'Post - PMCMV',
                db = 'mcmv') %>% 
  dplyr::select(id, db, period, class, uh) %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(lng = geometry[1],
                lat = geometry[2]) 


# * classification of real estate ----

re <- sf::st_read('data/housing/private/CEM_LancaRes_SP_2000_2013.shp', quiet= T) %>% 
  sf::st_transform(crsdegrees) %>% 
  dplyr::filter(ANO_LAN %in% 2002:2013, HOTEL==0, FLAT==0) %>% 
  dplyr::mutate(AR_UT_UNID = as.numeric(AR_UT_UNID),
                PC_TT_ATU = as.numeric(PC_TT_ATU),
                class = dplyr::case_when(AR_UT_UNID <= 50 & BANH_UNID == 1 & 
                                           GAR_UNID <= 1 & PC_TT_ATU <= 68429.16 ~ 'low', #88320.25  fase 1 68429.16
                                         AR_UT_UNID <= 70 & 
                                           BANH_UNID <= 2 & GAR_UNID <= 1 & 
                                           PC_TT_ATU <= 171072.90 ~ 'medium', # 273090.90 fase 1 171072.90
                                         TRUE ~ 'high'),
                class = factor(class, 
                               levels = c('low', 'medium', 'high'),
                               labels = c('low', 'medium', 'high')),
                ano = as.numeric(as.character(ANO_LAN)),
                period = cut(ano,
                             breaks=c(2001, 2008, 2014),
                             labels=c('Pre - PMCMV', 'Post - PMCMV'),
                             right = T,
                             include.lowest = F),
                db = 'regular real estate',
                id = as.character(ID)) %>% 
  dplyr::rename(uh = TT_UNID) %>% 
  dplyr::select(id, db, period, class, uh) %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(lng = geometry[1],
                lat = geometry[2])

# binding housing databases 
housing <- dplyr::bind_rows(mcmv, re) %>% 
  sf::st_as_sf(coords = c('lng', 'lat'))

rgdal::writeOGR(sf::as_Spatial(housing), 'data/housing/private/private_scs_paper.shp',
                layer = 'private', driver = 'ESRI Shapefile', overwrite_layer = T) 

# * creating matrix ----
p <- housing %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(lng = geometry[1],
                lat = geometry[2])

lims <- c(-46.826081, -46.365377, -23.984431, -23.384091)

# creating routine iteration
it <- expand.grid(cla = unique(p$class), per = unique(p$period), d = unique(p$db)) %>% 
  dplyr::filter(!(d == 'mcmv' & per == 'Pre - PMCMV') & 
                  !(d == 'mcmv' & cla == 'high') &
                  !(d == 'regular real estate' & cla == 'low'))

# routine 
matrix <- purrr::pmap(it, function(cla, per, d){
  message(cla, ' - ', per, ' - ', d)
  message('subsetting')
  plot <- p %>% 
    dplyr::filter(class == cla, period == per, db == d)
  
  message('creating matrix')
  m <- kde2d.weighted(x = plot$lng, 
                      y = plot$lat, 
                      n = 1000, 
                      w = plot$uh,
                      lims = lims)
  
  list(x = m$x, y = m$y, z = m$z, class = cla, period = per, db = d)
  
})

# * defining breaks of isobands ----
max <- purrr::map_dfr(matrix, function(m){
  data.frame(max = max(m$z))
})

brk_low <- (0:6)*max(max$max)/7
brk_high <- (1:7)*max(max$max)/7

# * creating isobands ----
bands <- purrr::map_dfr(matrix, function(m){
  # procedure to not even process the null bands
  max <- max(m$z)
  i <- sum(max > brk_low)
  
  b <- isoband::isobands(m$x, m$y, t(m$z),
                         levels_low = brk_low[1:i],
                         levels_high = brk_high[1:i])
  
  bands <- isoband::iso_to_sfg(b)
  bands <- tibble::tibble(level = brk_low[1:i],
                          class = m$class,
                          period = m$period,
                          db = m$db,
                          geometry = sf::st_as_sfc(bands))
  
})

bands_sf <- bands %>% 
  dplyr::filter(level!=0) %>% 
  sf::st_as_sf(crs = crsdegrees) %>% 
  sf::st_intersection(msp)

rgdal::writeOGR(sf::as_Spatial(bands_sf), 'data/housing/private/private_isobands_paper.shp', 
                layer = 'isobands', driver = 'ESRI Shapefile')
