# Library ----
library(magrittr)
base <- 210
quant <-  5
source('rscript/00_Maps_eng.R')

# Reading files ----

# accessibility
acc_org <- readr::read_csv('results/02_housing_acc_segreg/table/accessibility_distances.csv') 

acc <- acc_org %>% 
  dplyr::mutate(threshold = paste0(threshold, 'min')) %>% 
  dplyr::filter(threshold=='60min', year %in% c(2017)) %>% 
  dplyr::select(origin, year, perc) %>% 
  dplyr::rename(id = origin,
                acc = perc)

# zeis
zeis02 <- sf::st_read('data/shp/zones/zeis_2002_clean.shp', quiet = T) %>% 
  dplyr::mutate(zeis = as.numeric(stringr::str_replace_all(Layer, 'ZEIS\\_|\\_LIMITE','')),
                id = 1:dplyr::n()) %>% 
  dplyr::select(id, zeis) %>% 
  sf::st_transform(prjdegrees) 

zeis <- zeis02  %>% 
  dplyr::filter(zeis!=1) %>% 
  sf::st_intersection(grid) %>% 
  dplyr::rename(id_grid = id.1) %>% 
  dplyr::left_join(acc, by = c('id_grid' = 'id'))

zeis$area <- sf::st_area(sf::st_transform(zeis, crs = crsutm))

# housing
housing <- sf::read_sf('data/housing/private/private_scs_paper.shp', 
                       crs = crsdegrees, quiet = T) %>% 
  sf::st_intersection(grid) %>% 
  dplyr::rename(id_grid = id.1)%>% 
  dplyr::mutate(period = factor(period,
                                levels = c('Pre - PMCMV', 'Post - PMCMV')),
                class = factor(class, 
                               levels = c('low', 'medium', 'high'))) %>% 
  dplyr::left_join(acc, by = c('id_grid' = 'id'))

int_housing <- sf::st_intersection(housing, zeis) %>% 
  dplyr::rename(id_zeis = id.1) 

# parameters ----

# scenarios
scenes <- dplyr::tibble(sc = c('base', 1:12),
                        nhis = c('-', rep(rep(c('max', c('min', 'min')), each = 2), 2)),
                        nhmp = c('-', rep(rep(c('-', 'max', 'min'), each = 2), 2)),
                        nca = c('-', rep(rep(c('basic', 'max'), 3), 2)),
                        cnt = c('-', rep(c('his', 'max'), each = 6)))

# zeis parameters 

# Lei nº 13.885, de 25 de agosto de 2004
zeis_par <- data.frame(zeis = 1:4,
                       # quadro 2j
                       to = c(.5, .5, .7, .5),
                       ca_basic = 1,
                       ca_max = c(2.5, 2.5, 4, 1),
                       # art. 139 II, art. 140 II
                       his_max = 1,
                       his_min = c(.5, .5, .4, .7),
                       hmp_max = c(.5, .5, .6, .3),
                       hmp_min = c(.3, .4, .4, 0)) %>% 
  tidyr::pivot_longer(cols = c('ca_basic', 'ca_max'), 
                      names_to = 'nameca', names_prefix = 'ca_', 
                      values_to = 'ca') %>% 
  tidyr::pivot_longer(cols = c('his_min', 'his_max'), 
                      names_to = 'namehis', names_prefix = 'his_', 
                      values_to = 'his') %>% 
  tidyr::pivot_longer(cols = c('hmp_min', 'hmp_max'), 
                      names_to = 'namehmp', names_prefix = 'hmp_', 
                      values_to = 'hmp') %>% 
  dplyr::filter(!(namehis == 'max' & namehmp == 'max')) %>% 
  dplyr::mutate(hmp = ifelse(namehis == 'max', 0, hmp))

# processing potential constructive area and housing units----

pot <- zeis %>% 
  sf::st_drop_geometry() %>% 
  dplyr::filter(zeis %in% 2:3) %>% 
  dplyr::left_join(zeis_par) %>% 
  dplyr::mutate(a_to = units::drop_units(area)*to,
                a_comp = ca*units::drop_units(area),
                andares = a_comp/a_to,
                ida = paste0(id,id_grid),
                HIS = round(.6*his*a_comp/37),
                HMP = round(.6*hmp*a_comp/46),
                alto = round(.6*(1-his-hmp)*a_comp/90),
                period = ifelse(year == 2010, 'fase 1', 'fase 2'),
                id = id_grid) %>% 
  dplyr::select(id, ida, zeis, period, acc, nameca, namehis, namehmp,  his, hmp, HIS, HMP, alto)

# processing the scenarios ----
scenarios <- purrr::pmap_dfr(scenes[-1,], function(sc, nhis, nhmp, nca, cnt){
  message('scenario ', sc)
  nhmp <- ifelse(nhmp=='-', 'min', nhmp)
  message(nhis,' - ', nhmp, ' - ', nca, ' - ', cnt)
  
  housing_org <- housing %>% 
    sf::st_drop_geometry()
  pot_org <- pot
  
  # processes the amount of housing to emulate
  housing_tot <- housing_org  %>% 
    dplyr::group_by(class) %>% 
    dplyr::summarise(uh = sum(uh)) 
  
  # filters the potential units for the scenario
  p <- dplyr::filter(pot_org, namehis == nhis, namehmp == nhmp, nameca == nca) 
  
  # total HIS HU to relocate
  if(cnt == 'his'){
    tot <- dplyr::filter(housing_tot, class == 'low')$uh
  } else {
    tot <- Inf
  }
  
  # prepare the relocated housing units
  prep <- p %>% 
    dplyr::arrange(desc(acc)) %>% 
    dplyr::mutate(cum = cumsum(HIS),
                  dif = tot - cum) %>% 
    dplyr::filter(cum <= tot | dplyr::lag(dif) > 0 ) %>% 
    dplyr::mutate(scenario = sc) %>% 
    dplyr::select(acc, scenario, HIS, HMP, alto, zeis, ida, id)
  
  # the low class housing units
  his <- prep %>% 
    dplyr::mutate(class = 'low',
                  uh = HIS) %>% 
    dplyr::select(acc, scenario, class, uh, zeis, ida, id)
  
  # processing the medium  and high class
  if(nhis == 'max'){
    
    # if at zeis should be built social housing, 
    # the medium class and higher class will not be relocated
    hmp <- housing_org %>% 
      dplyr::filter(class %in% c('medium', 'high')) %>% 
      dplyr::mutate(scenario = sc) %>% 
      dplyr::select(acc, scenario, class, uh, id)
    
    i <- dplyr::bind_rows(his, hmp)
    
  } else {
    
    # part of the medium will be relocated,
    # but it will respect the maxixum value possible
    hmp_realoc <- prep %>% 
      dplyr::mutate(class = 'medium',
                    uh = HMP) %>% 
      dplyr::select(acc, scenario, class, uh, zeis, ida, id)
    
    # how many HU was relocated
    tot <- sum(hmp_realoc$uh)
    
    # processes which ones were not relocated
    hmp <- housing_org %>% 
      dplyr::filter(class == 'medium') %>% 
      dplyr::arrange(acc) %>% 
      dplyr::mutate(cum = cumsum(uh),
                    dif = tot - cum,
                    scenario = sc) %>% 
      dplyr::filter(dif < 0) %>% # chooses the ones with worst position
      dplyr::select(acc, scenario, class, uh, id) %>% 
      dplyr::bind_rows(hmp_realoc)
    
    i <- dplyr::bind_rows(his, hmp)
    
    if (nhmp == 'max') {
      
      # if at zeis should be built only social housing and medium class, 
      # the higher class will not be relocated
      alto <- housing_org %>% 
        dplyr::filter(class %in% c('high')) %>% 
        dplyr::mutate(scenario = sc) %>% 
        dplyr::select(acc, scenario, class, uh, id)
      
    } else {
      
      # part of the medium will be relocated,
      # but it will respect the maxixum value possible
      alto_realoc <- prep %>% 
        dplyr::mutate(class = 'high',
                      uh = alto) %>% 
        dplyr::select(acc, scenario, class, uh, zeis, ida, id)
      
      # how many HU was not relocated
      tot <-sum(alto_realoc$uh)
      
      # processes which one will not be relocated
      alto <- housing_org %>% 
        dplyr::filter(class == 'high') %>% 
        dplyr::arrange(acc) %>% 
        dplyr::mutate(cum = cumsum(uh),
                      dif = tot - cum,
                      scenario = sc) %>% 
        dplyr::filter(dif < 0) %>% 
        dplyr::select(acc, scenario, class, uh, id) %>% 
        dplyr::bind_rows(alto_realoc)
    }
    
    i <- dplyr::bind_rows(i, alto)
    
  } 
  
  dplyr::select(i, -ida)
  
})

readr::write_csv(scenarios, 'results/03_zeis/table/scenarios_paper.csv')
