# Library ----
library(magrittr)
base <- 210
quant <-  5
source('rscript/00_Maps.R')

# Reading files ----

# accessibility
acc_org <- readr::read_csv('results/02_housing_acc_segreg/table/accessibility_distances.csv') 

acc <- acc_org %>% 
  dplyr::mutate(threshold = paste0(threshold, 'min')) %>% 
  dplyr::filter(threshold=='60min', year %in% c(2010, 2014))

acc_aux <- acc %>% 
  dplyr::rename(id = origin) %>% 
  dplyr::select(id, year, perc) %>% 
  tidyr::pivot_wider(names_from = year, names_prefix = 'y_', values_from = 'perc')

# zeis
zeis02 <- sf::st_read('data/shp/zones/zeis_2002_clean.shp', quiet = T) %>% 
  dplyr::mutate(zeis = as.numeric(stringr::str_replace_all(Layer, 'ZEIS\\_|\\_LIMITE','')),
                id = 1:dplyr::n()) %>% 
  dplyr::select(id, zeis) %>% 
  sf::st_transform(prjdegrees) 

zeis <- zeis02 %>%
  dplyr::filter(zeis!=1) %>%
  sf::st_intersection(grid) %>%
  dplyr::rename(id_grid = id.1) %>%
  dplyr::left_join(acc_aux, by = c('id_grid' = 'id')) %>%
  dplyr::group_by(id, zeis) %>%
  dplyr::summarise(y_2010 = mean(y_2010, na.rm=T),
                   y_2014 = mean(y_2014, na.rm=T)) %>%
  dplyr::ungroup()

zeis$area <- sf::st_area(sf::st_transform(zeis, crs = crsutm))

# housing
private_org <- sf::st_read('data/housing/private/private.shp', quiet = T) 

private <- private_org %>% 
  sf::st_transform(prjdegrees) %>% 
  dplyr::rename(uh = TT_UNID,
                id = ID)  %>% 
  dplyr::mutate(id = as.character(id),
                class = factor(classe,
                               levels = c('lower', 'medium', 'higher'),
                               labels= c('HIS', 'HMP', 'alta renda')),
                period = stringr::str_replace_all(period, 'Stage', 'fase'),
                origin = 'privado') %>% 
  dplyr::select(id, period, class, uh, origin)

mcmv <- sf::st_read('data/shp/mcmv_sf.shp', quiet = T)%>% 
  sf::st_transform(prjdegrees) %>% 
  dplyr::rename(period = fase_mcmv,
                class = faixa) %>% 
  dplyr::filter(period != 'Stage 3') %>% 
  dplyr::mutate(class= dplyr::case_when(class == 1 ~ 'HIS',
                                         TRUE ~ 'HMP'),
                origin = 'mcmv',
                period = stringr::str_replace_all(period, 'Stage', 'fase'))

# intersection

private <- private %>% 
  sf::st_intersection(grid) %>% 
  dplyr::select(-id) %>% 
  dplyr::rename(id = id.1)

housing <- rbind(mcmv, private) %>%
  dplyr::left_join(acc_aux) %>% 
  dplyr::mutate(acc = dplyr::case_when(period == 'fase 1' ~ y_2010,
                                       TRUE ~ y_2014)) %>% 
  dplyr::select(-y_2010, -y_2014)

int_housing <- sf::st_intersection(housing, zeis) %>% 
  dplyr::rename(id_zeis = id.1) 

# * parameters ----

# scenarios
scenes <- dplyr::tibble(sc = c('base', 1:6),
                        nhis = c('-', rep(c('max', 'min'), each = 2), rep('min', 2)),
                        nhmp = c(rep('-', 3), rep('max', 2), rep('min', 2)),
                        nca = c('-', rep(c('basic', 'max'), 3)))

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

# * processing potential constructive area and housing units----

zeis <- zeis02  %>% 
  dplyr::filter(zeis!=1) %>% 
  sf::st_intersection(grid) %>% 
  dplyr::rename(id_grid = id.1) %>% 
  dplyr::left_join(acc, by = c('id_grid' = 'origin')) %>% 
  dplyr::mutate(acc = perc) 

zeis$area <- sf::st_area(sf::st_transform(zeis, crs = crsutm))

pot <- zeis %>% 
  sf::st_drop_geometry() %>% 
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

scenarios <- purrr::pmap_dfr(scenes[-1,], function(sc, nhis, nhmp, nca){
  message('scenario ', sc)
  nhmp <- ifelse(nhmp=='-', 'min', nhmp)
  message(nhis,' - ', nhmp, ' - ', nca)
  
  housing_org <- housing %>% 
    sf::st_drop_geometry()
  pot_org <- pot
  
  # processes the amount of housing to emulate
  housing_tot <- housing_org  %>% 
    dplyr::group_by(class, period) %>% 
    dplyr::summarise(uh = sum(uh)) 
  
  # filters the potential units for the scenario
  p <- dplyr::filter(pot_org, namehis == nhis, namehmp == nhmp, nameca == nca) 
  
  # initialize the used zeis 
  h <<- data.frame(period = character(),
                   acc = double(),
                   scenario = character(), 
                   class = character(), 
                   uh = double(),
                   zeis = double(),
                   ida= character(),
                   stringsAsFactors = F)
  
  purrr::map_dfr(c('fase 1', 'fase 2'), function(per){
    
    # total HIS HU to relocate
    tot <- dplyr::filter(housing_tot, period == per, class == 'HIS')$uh
    
    # prepare the relocated housing units
    prep <- p %>% 
      dplyr::filter(period == per, !(ida %in% h$ida)) %>% 
      dplyr::arrange(desc(acc)) %>% 
      dplyr::mutate(cum = cumsum(HIS),
                    dif = tot - cum) %>% 
      dplyr::filter(cum <= tot | dplyr::lag(dif) > 0 ) %>% 
      dplyr::mutate(scenario = sc) %>% 
      dplyr::select(period, acc, scenario, HIS, HMP, alto, zeis, ida, id)
    
    # the low class housing units
    his <- prep %>% 
      dplyr::mutate(class = 'HIS',
                    uh = HIS) %>% 
      dplyr::select(period, acc, scenario, class, uh, zeis, ida, id)
    
    # processing the medium class
    if(nhis == 'max'){
      
      # if at zeis should be built social housing, 
      # the medium class and higher class will not be relocated
      hmp <- housing_org %>% 
        dplyr::filter(class %in% c('HMP', 'alta renda'), period == per) %>% 
        dplyr::mutate(scenario = sc) %>% 
        dplyr::select(period, acc, scenario, class, uh, id)
      
      i <- dplyr::bind_rows(his, hmp)
      
    } else {
      
      # part of the medium will be relocated,
      # but it will respect the maxixum value possible
      hmp_realoc <- prep %>% 
        dplyr::mutate(class = 'HMP',
                      uh = HMP) %>% 
        dplyr::select(period, acc, scenario, class, uh, zeis, ida, id)
      
      # how many HU was relocated
      tot <- sum(hmp_realoc$uh)
      
      # processes which ones was not relocated
      hmp <- housing_org %>% 
        dplyr::filter(period == per, class == 'HMP') %>% 
        dplyr::arrange(acc) %>% 
        dplyr::mutate(cum = cumsum(uh),
                      dif = tot - cum,
                      scenario = sc) %>% 
        dplyr::filter(dif < 0) %>% # chooses the ones with worst position
        dplyr::select(period, acc, scenario, class, uh, id) %>% 
        dplyr::bind_rows(hmp_realoc)
      
      i <- dplyr::bind_rows(his, hmp)
      
      if (nhmp == 'max') {
        
        # if at zeis should be built only social housing and medium class, 
        # the higher class will not be relocated
        alto <- housing_org %>% 
          dplyr::filter(class %in% c('alta renda'), period == per) %>% 
          dplyr::mutate(scenario = sc) %>% 
          dplyr::select(period, acc, scenario, class, uh, id)
        
      } else {
        
        # part of the medium will be relocated,
        # but it will respect the maxixum value possible
        alto_realoc <- prep %>% 
          dplyr::mutate(class = 'alta renda',
                        uh = alto) %>% 
          dplyr::select(period, acc, scenario, class, uh, zeis, ida, id)
        
        # how many HU was not relocated
        tot <-sum(alto_realoc$uh)
        
        # processes which one will not be relocated
        alto <- housing_org %>% 
          dplyr::filter(period == per, class == 'alta renda') %>% 
          dplyr::arrange(acc) %>% 
          dplyr::mutate(cum = cumsum(uh),
                        dif = tot - cum,
                        scenario = sc) %>% 
          dplyr::filter(dif < 0) %>% 
          dplyr::select(period, acc, scenario, class, uh, id) %>% 
          dplyr::bind_rows(alto_realoc)
      }
      
      i <- dplyr::bind_rows(i, alto)
      
    } 
    
    h <<- dplyr::bind_rows(h, i)
    
    dplyr::select(i, -ida)
  })
})

readr::write_csv(scenarios, 'results/03_zeis/table/scenarios.csv')
