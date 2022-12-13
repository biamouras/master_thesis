#' Plots figures 18-28

# Library ----

library(tidyverse)
library(ggnewscale)
library(treemapify)
library(gtable)
library(grid)
library(spatstat)
library(data.table)
base <- 210
quant <-  5
source('rscript/00_Maps.R')

# Reading files----

# * accessibility ----
acc_org <- readr::read_csv('results/02_housing_acc_segreg/table/accessibility_distances.csv') 

acc <- acc_org %>% 
  dplyr::mutate(threshold = paste0(threshold, 'min')) %>% 
  dplyr::filter(threshold=='60min', year %in% c(2010, 2014))

acc_aux <- acc %>% 
  dplyr::rename(id = origin) %>% 
  dplyr::select(id, year, perc) %>% 
  tidyr::pivot_wider(names_from = year, names_prefix = 'y_', values_from = 'perc')

# * population ----
pop_micro <- readr::read_csv('data/population/population_micro_grid2.csv') 

# * housing ----
private_org <- sf::st_read('data/housing/private/private.shp', quiet = T) 

# % of useful area and total area 
private_org %>% 
  dplyr::mutate(p1 = AU_EMP/AT_EMP,
                p2 = AR_UT_UNID*TT_UNID/AT_EMP) %>% 
  sf::st_drop_geometry() %>% 
  dplyr::group_by(classe) %>% 
  dplyr::summarise(p1_mean = mean(p1, na.rm = T),
                   p2_mean = mean(p2, na.rm = T),
                   au_mean = mean(AR_UT_UNID, na.rm = T))

private <- private_org %>% 
  sf::st_transform(prjdegrees) %>% 
  dplyr::rename(uh = TT_UNID,
                id = ID)  %>% 
  dplyr::mutate(id = as.character(id),
                class = dplyr::case_when(classe == 'lower' ~ 'HIS', 
                                         classe == 'medium' ~ 'HMP',
                                         TRUE ~ 'alta renda'),
                period = stringr::str_replace_all(period, 'Stage', 'fase'),
                origin = 'privado') %>% 
  dplyr::select(id, period, class, uh, origin)

mcmv <- sf::st_read('data/shp/mcmv_sf.shp', quiet = T)%>% 
  sf::st_transform(prjdegrees) %>% 
  dplyr::rename(period = fase_mcmv,
                class = faixa) %>% 
  dplyr::filter(period != 'Stage 3') %>% 
  dplyr::mutate(class = dplyr::case_when(class == 1 ~ 'HIS',
                                         TRUE ~ 'HMP'),
                origin = 'mcmv',
                period = stringr::str_replace_all(period, 'Stage', 'fase'))

# intersection

private <- private %>% 
  sf::st_intersection(grid) %>% 
  dplyr::select(-id) %>% 
  dplyr::rename(id = id.1) 

private_df <- private %>% 
  sf::st_drop_geometry()

housing <- rbind(mcmv, private) %>%
  dplyr::left_join(acc_aux) %>% 
  dplyr::mutate(acc = dplyr::case_when(period == 'fase 1' ~ y_2010,
                                       TRUE ~ y_2014)) %>% 
  dplyr::select(-y_2010, -y_2014)

# * zeis ----
zeis02 <- sf::st_read('data/shp/zones/zeis_2002_clean.shp', quiet = T) %>% 
  dplyr::mutate(zeis = as.numeric(stringr::str_replace_all(Layer, 'ZEIS\\_|\\_LIMITE','')),
                id = 1:dplyr::n()) %>% 
  dplyr::select(id, zeis) %>% 
  sf::st_transform(prjdegrees) 

zeis02_sf <- zeis02 %>% 
  dplyr::mutate(year = 'PDE 2002') 

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

# * scenarios ---- 
scenarios <- readr::read_csv('results/03_zeis/table/scenarios.csv') %>% 
  dplyr::mutate(class = factor(class,
                               levels = c('HIS', 'HMP', 'alta renda')))

trilho_full <- trilhos

trilhos <- trilhos %>%
  dplyr::filter(ano %in% c(2010, 2014)) %>% 
  dplyr::mutate(period = ifelse(ano == 2010,'fase 1', 'fase 2'))


# ZEIS ----
# numeric analysis:  how many zeis are used? ----

int_housing <- sf::st_intersection(housing, zeis) %>% 
  dplyr::rename(id_zeis = id.1) 

out_housing <- housing %>% 
  dplyr::filter(!(id %in% int_housing$id))

zeis <- sf::st_drop_geometry(zeis)
zeis$used <- 0
zeis[zeis$id %in% int_housing$id_zeis, 'used'] <- 1

zeis %>% 
  dplyr::group_by(used) %>% 
  dplyr::summarise(n = dplyr::n(),
                   area = sum(area)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(n = n/sum(n),
                area = area/sum(area))
zeis %>% 
  dplyr::group_by(zeis, used) %>% 
  dplyr::summarise(n = dplyr::n(),
                   area = sum(area, na.rm = T)) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(zeis) %>% 
  dplyr::mutate(pn = n/sum(n),
                parea = area/sum(area)) 

# figure 24: location of zeis ----

zeis %>% 
  dplyr::mutate(zeis = paste('zeis', zeis)) %>% 
  tidyr::pivot_longer(cols = c('y_2010', 'y_2014'), names_to = 'year', names_prefix = 'y_',
                      values_to = 'acc') %>% 
  ggplot() +
  geom_boxplot(aes(zeis, acc, fill = zeis), 
               alpha = 0.3, color = 'grey40', lwd = line.size*.005, show.legend = F) +
  scale_fill_manual(values = colOrange(5)[2:4])+
  scale_y_continuous(labels = scales::percent, limits = c(0,1), expand = c(0,0))+
  labs(x = '', y = '% de total de empregos') +
  facet_grid(~year) +
  theme_chart

size <- dplyr::filter(map_dim, map=='chart')
ggsave('latex/dissertacao_bms_vfull/figuras/resultados/zeis/boxplot_zeis.png',
       dpi = 500, type = 'cairo-png', width = size$width, height = size$height, units = 'mm')

# figure 25: how were they used? ----

int_housing %>% 
  sf::st_drop_geometry() %>% 
  dplyr::group_by(class, zeis) %>% 
  dplyr::summarise(uh = sum(uh)) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(zeis) %>% 
  dplyr::mutate(p_uh = uh/sum(uh),
                class = paste0(class, ' - ', round(p_uh*100), '%')) %>% 
  dplyr::ungroup() %>% 
  dplyr::select(zeis, class, uh, p_uh) %>% 
  dplyr::mutate(zeis = paste('zeis', zeis)) %>% 
  ggplot(aes(area = uh, subgroup = zeis, fill = zeis)) +
  geom_treemap(start = 'topleft', alpha = 0.3, color = 'grey40', show.legend = F) +
  geom_treemap_subgroup_text(start = 'topleft',place = 'topleft') +
  geom_treemap_text(start = 'topleft',aes(label = class), place = 'bottomright') +
  scale_fill_manual(values = colOrange(5)[2:4])

size <- dplyr::filter(map_dim, map=='chart')
ggsave('latex/dissertacao_bms_vfull/figuras/03_resultados/02_zeis/tree_zeis_used.png',
       dpi = 500, type = 'cairo-png', width = size$width, height = size$height, units = 'mm')


# figure 26: comparing not used zeis to his and hmp out of zeis ----

zeis_not <- zeis %>% 
  dplyr::filter(used == 0, zeis != 4) %>% 
  tidyr::pivot_longer(cols = c('y_2010', 'y_2014'), 
                      names_to = 'year', names_prefix = 'y_', values_to = 'acc') %>% 
  dplyr::mutate(class = 'zeis não usadas',
                period = ifelse(year == 2010, 'fase 1', 'fase 2'))%>% 
  dplyr::select(class, period, acc) 

out_housing %>% 
  dplyr::filter(class != 'alta renda') %>% 
  sf::st_drop_geometry() %>% 
  dplyr::select(class, period, acc) %>% 
  dplyr::bind_rows(zeis_not) %>% 
  ggplot() +
  geom_boxplot(aes(class, acc, fill = class), 
               alpha = 0.3, color = 'grey40', lwd = line.size*.005) +
  scale_fill_manual('', breaks = c('HIS', 'HMP', 'zeis não usadas'),
                    values = colOrange(5)[c(1,3,4)]) +
  scale_y_continuous(labels = scales::percent, limits = c(0,1), expand = c(0,0)) +
  labs(x='', y='% do total de empregos',
       caption = 'fase 1: 2009-2010; fase 2: 2011-2014') + 
  facet_grid(~period)+
  theme_chart +
  theme(legend.position = 'bottom')

size <- dplyr::filter(map_dim, map=='chart')
ggsave('latex/dissertacao_bms_vfull/figuras/03_resultados/02_zeis/boxplot_zeis_used.png',
       dpi = 500, type = 'cairo-png', width = size$width, height = size$height, units = 'mm')

# SCENARIOS ----
# scenarios
scenes <- dplyr::tibble(sc = c('base', 1:6),
                        nhis = c('-', rep(c('max', 'min'), each = 2), rep('min', 2)),
                        nhmp = c(rep('-', 3), rep('max', 2), rep('min', 2)),
                        nca = c('-', rep(c('basic', 'max'), 3)))

# new figure uh by zeis and scenarios ----

tot <- scenarios %>% 
  dplyr::group_by(scenario, class) %>% 
  dplyr::summarise(uh = sum(uh)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(scenario = factor(paste('cenário', scenario),
                                  levels = paste('cenário', c(1,3,5,2,4,6)))) 

ptot <- scenarios %>% 
  dplyr::group_by(scenario, zeis, class) %>% 
  dplyr::summarise(uh = sum(uh)) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(scenario, class) %>% 
  dplyr::mutate(puh = uh/sum(uh)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(zeis = ifelse(is.na(zeis), 0, zeis),
                zeis = factor(zeis,
                              levels = c(0, 2, 3))) %>% 
  dplyr::select(-uh) %>% 
  dplyr::group_by(scenario, class) %>% 
  dplyr::arrange(desc(zeis)) %>% 
  dplyr::mutate(pos = cumsum(puh)+ ifelse(puh<0.05, 0.03, -0.03)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate( scenario = factor(paste('cenário', scenario),
                                   levels = paste('cenário', c(1,3,5,2,4,6)))) 

p <- scenarios %>% 
  dplyr::group_by(scenario, zeis, class) %>% 
  dplyr::summarise(uh = sum(uh)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(zeis = ifelse(is.na(zeis), 0, zeis),
                zeis = factor(zeis,
                              levels = c(0, 2, 3)),
                scenario = factor(paste('cenário', scenario),
                                  levels = paste('cenário', c(1,3,5,2,4,6)))) %>%
  ggplot() +
  geom_bar(aes(x = class, y = uh, fill = zeis), 
           stat = 'identity', position = position_fill()) +
  geom_text(data = tot, aes(x = class, 1.03, 
                            label = format(uh, digits=4,big.mark='.')),
            size = font.size*0.03, color = 'grey20') +
  geom_text(data = ptot, aes(x = class, pos, 
                             label = scales::percent(puh, decimal.mark = ',')),
            size = font.size*0.03, color = 'grey80') +
  scale_fill_manual('',
                    values = c(colRedBlue(2)[2], colOrange(5)[2:3]),
                    breaks = c(0, 2, 3),
                    labels = c('não realocada', 'zeis 2', 'zeis 3')) +
  scale_y_continuous(labels = scales::percent, 
                     expand = expansion(mult = c(0, 0.05))) +
  labs(x = '', y = '% de UH') +
  facet_wrap(.~scenario) +
  theme_chart

p

size <- dplyr::filter(map_dim, map=='chart')
ggsave(paste0('latex/dissertacao_bms_vfull/figuras/03_resultados/02_zeis/barplot_relocation.png'),
       p, dpi = 500, width = size$width, height = size$width*0.65, units = "mm")

# * binding base scenario to processed ones ----

base_sc <- housing %>% 
  sf::st_drop_geometry() %>% 
  dplyr::mutate(scenario = 'cenário base') %>% 
  dplyr::select(scenario, period, class, uh, acc)

df <- scenarios %>% 
  dplyr::mutate(scenario = paste('cenário', scenario)) %>% 
  dplyr::filter(uh > 0) %>% 
  dplyr::select(scenario, period, class, uh, acc) %>% 
  dplyr::bind_rows(base_sc) 

# figure 27: boxplot scenarios ----

df %>% 
  dplyr::mutate(`cenário` = stringr::str_remove(scenario, 'cenário '),
                `cenário` = factor(`cenário`,
                                   levels = c('base', 1:6)),
                classe = factor(class,
                                levels = c('HIS', 'HMP', 'alta renda'))) %>% 
  ggplot() +
  geom_boxplot(aes(`cenário`, acc, fill = classe), 
               alpha = 0.3, color = 'grey40', lwd = line.size*.005) +
  scale_fill_manual('classe',
                    breaks = c('HIS', 'HMP', 'alta renda'),
                    values = colOrange(5)[c(1,3,5)]) +
  scale_y_continuous(labels = scales::percent, limits = c(0,1), expand = c(0,0)) +
  labs(y='% total de empregos',
       caption = 'fase 1: 2009-2010; fase 2: 2011-2014') + 
  facet_grid(~period) +
  theme_chart +
  theme(axis.ticks.x = element_blank())

size <- dplyr::filter(map_dim, map=='chart')
ggsave(paste0('latex/dissertacao_bms_vfull/figuras/03_resultados/02_zeis/boxplot_scenarios.png'),
       dpi = 500, width = size$width, height = size$height, units = "mm")

# figure 28: gini ----

# * gini base ----
df_lor <- housing %>% 
  dplyr::filter(!is.na(acc)) %>% 
  dplyr::group_by(period) %>% 
  dplyr::arrange(acc) %>% 
  dplyr::mutate(cumuh = cumsum(uh)/sum(uh, na.rm = T),
                cumacc = cumsum(acc*uh)/sum(acc*uh, na.rm = T)) %>% 
  dplyr::ungroup()

median <- df_lor %>% 
  dplyr::group_by(period, class, origin) %>% 
  dplyr::summarise(median.x = weighted.median(cumuh, uh),
                   median.y = weighted.median(cumacc, uh)) %>% 
  dplyr::ungroup()

gini_base <- df_lor %>% 
  dplyr::filter(!is.na(cumacc)) %>% 
  dplyr::group_by(period) %>% 
  dplyr::arrange(acc) %>% 
  dplyr::mutate(gini1 = (dplyr::lead(cumuh)-cumuh)*(dplyr::lead(cumacc)+cumacc)) %>% 
  dplyr::filter(!is.na(gini1)) %>% 
  dplyr::summarise(gini = 1 - sum(gini1))

# * gini scenarios ----

sc <- scenes
sc$tipo <- c('-',
             rep('máx. HIS', 2),
             rep('máx. HMP',2),
             rep('máx. alta renda',2))

sc <- sc %>% 
  dplyr::mutate(nca = ifelse(nca == 'basic', 'CA básico', 'CA máximo'),
                tipo = factor(tipo,
                              levels = c('máx. HIS',
                                         'máx. HMP',
                                         'máx. alta renda')),
                nhis = factor(nhis,
                              levels = c('max', 'min'),
                              labels = c('máx. HIS', 'min. HIS')),
                nhmp = factor(nhmp,
                              levels = c('-', 'max', 'min'),
                              labels = c('-', 'máx. HMP', 'min. HMP')),
                sc = paste('cenário', sc))

df_lor <- df %>% 
  dplyr::filter(!is.na(acc))  %>% 
  dplyr::group_by(period, scenario) %>% 
  dplyr::arrange(acc) %>% 
  dplyr::mutate(cumuh = cumsum(uh)/max(cumsum(uh), na.rm = T),
                cumacc = cumsum(acc*uh)/max(cumsum(acc*uh), na.rm = T)) %>% 
  dplyr::ungroup() %>% 
  dplyr::filter(scenario != 'cenário base') %>% 
  dplyr::left_join(sc, by = c('scenario' = 'sc'))

median <- df_lor %>% 
  dplyr::group_by(period, class, scenario) %>% 
  dplyr::summarise(median.x = weighted.median(cumuh, uh),
                   median.y = weighted.median(cumacc, uh)) %>% 
  dplyr::ungroup()%>% 
  dplyr::filter(scenario != 'cenário base') %>% 
  dplyr::left_join(sc, by = c('scenario' = 'sc'))

gini <- df_lor %>% 
  dplyr::filter(!is.na(cumacc)) %>% 
  dplyr::group_by(period, scenario) %>% 
  dplyr::arrange(acc) %>% 
  dplyr::mutate(gini1 = (dplyr::lead(cumuh)-cumuh)*(dplyr::lead(cumacc)+cumacc)) %>% 
  dplyr::filter(!is.na(gini1)) %>% 
  dplyr::summarise(gini = 1 - sum(gini1)) %>% 
  dplyr::filter(scenario != 'cenário base') %>% 
  dplyr::left_join(sc, by = c('scenario' = 'sc'))

g <- gini_base %>% 
  sf::st_drop_geometry() %>% 
  dplyr::mutate(scenario = 'cenário base') %>% 
  dplyr::bind_rows(gini)

gb_1 <- dplyr::filter(g, scenario == 'cenário base', period == 'fase 1') %>% 
  .$gini
gb_2 <- dplyr::filter(g, scenario == 'cenário base', period == 'fase 2') %>% 
  .$gini

gtable <- g %>% 
  tidyr::pivot_wider(names_from = 'period', values_from = 'gini') 

p <- gtable %>% 
  dplyr::mutate(scenario = factor(scenario,
                                  levels = rev(paste('cenário', c('base', 1:6))))) %>% 
  ggplot() +
  geom_vline(xintercept = gb_1 , color="grey50", 
             lwd = line.size*.006, lty = 'dashed') +
  geom_vline(xintercept = gb_2 , color="grey50", 
             lwd = line.size*.006, lty = 'dashed') +
  geom_segment(aes(x = `fase 1`, xend = `fase 2`, y=scenario, yend=scenario)) +
  geom_point(data = g, aes(x = gini, y = scenario, color = period), size = 3, show.legend = T) +
  scale_color_manual(breaks = c('fase 1', 'fase 2'), 
                     values = c('fase 1' = colRedBlue(5)[5],
                                'fase 2' = colRedBlue(5)[1]),
                     guide = guide_legend(title= "", 
                                          order = 1,
                                          title.position = "top")) +
  scale_x_continuous(labels = scales::label_number(decimal.mark = ',', big.mark = '.')) +
  coord_cartesian(xlim=c(0.1, .4), ylim = c(.25, 7.75), expand = F)+
  labs(x = 'gini', y = '', caption = 'fase 1: 2009-2010; fase 2: 2011-2014') +
  theme_chart +
  theme(legend.position = 'bottom',
        plot.margin = margin(2, 7.5, 2, 2, 'mm'))

p
size <- dplyr::filter(map_dim, map=='chart')
ggsave("latex/dissertacao_bms_vfull/figuras/03_resultados/02_zeis/gini_scenarios.png", 
       p, dpi = 500, width = size$width, height = size$height, units = "mm")

# figure 29: lorenz scenarios ----

p <- df_lor %>% 
  dplyr::mutate(class = factor(class,
                               levels = c('HIS', 'HMP', 'alta renda'))) %>% 
  ggplot() +
  geom_line(aes(x=cumuh, y=cumacc), 
            lwd = line.size*.008)+
  geom_abline(intercept=0, slope=1, color="grey50", 
              lwd = line.size*.006, lty = 'dashed') +
  geom_point(data = median, aes(x = median.x, y = median.y, color = class), size = 3) +
  geom_text(data = gini, aes(x = 0.3, y = 0.95, 
                             label = paste0('Gini: ', 
                                            format(gini,
                                                   digits=4,
                                                   decimal.mark=',')))) +
  scale_color_manual('classe',
                     breaks = c('HIS', 'HMP', 'alta renda'),
                     values = colOrange(5)[c(1,3,5)]) +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  labs(x='UH', y='acessibilidade',
       caption = 'fase 1: 2009-2010; fase 2: 2011-2014') + 
  coord_fixed(xlim=c(0,1), ylim=c(0,1), expand = F)+
  ggh4x::facet_nested(tipo  ~ nca + period)+
  theme_chart +
  theme(panel.spacing = unit(7.5, 'mm'),
        plot.caption.position = 'plot')
#bs + 
p
size <- dplyr::filter(map_dim, map=='chart')
ggsave("latex/dissertacao_bms_vfull/figuras/03_resultados/02_zeis/lorenz_scenarios.png", 
       p, dpi = 500, width = size$width, height = size$width*.8, units = "mm")

# lorenz of municipality ----

# * preparing data ----
sc <- scenes
sc$tipo <- c('-',
             rep('máx. HIS', 2),
             rep('máx. HMP',2),
             rep('máx. alta renda',2))

sc <- sc %>% 
  dplyr::mutate(nca = ifelse(nca == 'basic', 'CA básico', 'CA máximo'),
                tipo = factor(tipo,
                              levels = c('máx. HIS',
                                         'máx. HMP',
                                         'máx. alta renda')),
                nhis = factor(nhis,
                              levels = c('max', 'min'),
                              labels = c('máx. HIS', 'min. HIS')),
                nhmp = factor(nhmp,
                              levels = c('-', 'max', 'min'),
                              labels = c('-', 'máx. HMP', 'min. HMP')),
                sc = paste('cenário', sc))


pop <- merge(pop_micro, data.frame(scenario = paste('cenário', c(1:6, 'base', 'original')))) %>% 
  dplyr::rename(id = id_grid) %>% 
  tidyr::pivot_longer(g1:g3, 'group', values_to = 'uh') %>% 
  dplyr::mutate(class = 'domicílio')

base_sc <- housing %>% 
  sf::st_drop_geometry() %>% 
  dplyr::mutate(scenario = 'cenário base') %>% 
  dplyr::select(scenario, period, class, uh, acc, id) 

df_lor <- scenarios %>% 
  dplyr::mutate(scenario = paste('cenário', scenario)) %>% 
  dplyr::bind_rows(base_sc) %>% 
  dplyr::mutate(group = dplyr::case_when(class == 'HIS' ~ 'g1',
                                         class == 'HMP' ~ 'g2',
                                         TRUE ~ 'g3'),
                class = 'UH') %>% 
  dplyr::select(-acc, -zeis, -period) %>% 
  dplyr::bind_rows(pop) %>% 
  dplyr::left_join(dplyr::filter(acc, year == 2010), by = c('id' = 'origin')) %>%
  dplyr::select(-acc, -threshold, -year, -mtype, -accessibility) %>% 
  dplyr::mutate(scenario = factor(scenario,
                                  levels = paste('cenário', c('original', 'base', 1:6)))) %>% 
  dplyr::filter(!is.na(perc), !is.na(uh)) %>% 
  dplyr::group_by(scenario) %>% 
  dplyr::arrange(perc) %>% 
  dplyr::mutate(cumuh = cumsum(uh)/sum(uh, na.rm = T),
                cumacc = cumsum(perc*uh)/sum(perc*uh, na.rm = T)) %>% 
  dplyr::ungroup()

# figure 30: gini of municipality ----

gini <- df_lor %>%  
  dplyr::group_by(scenario) %>% 
  dplyr::arrange(perc) %>% 
  dplyr::mutate(gini1 = (dplyr::lead(cumuh)-cumuh)*(dplyr::lead(cumacc)+cumacc)) %>% 
  dplyr::filter(!is.na(gini1)) %>% 
  dplyr::summarise(gini = 1 - sum(gini1)) 

gini_org <- gini %>% 
  dplyr::filter(scenario == 'cenário original') %>% 
  .$gini

gini %>% 
  dplyr::filter(scenario != 'cenário original') %>% 
  dplyr::select(scenario, gini) %>% 
  ggplot(aes(x = scenario, y = gini)) +
  geom_hline(aes(yintercept = gini_org), color="grey50", 
             lwd = line.size*.006, lty = 'dashed') +
  geom_segment(aes(x = scenario, xend = scenario, y=gini_org, yend = gini)) +
  geom_point(color = colOrange(5)[3], size = 3) +
  geom_text(aes(label = format(gini, digits = 4, decimal.mark = ',')), nudge_x = .3)+
  geom_text(x = 7.2, y = gini_org+.001, 
            label = paste('original: ', format(gini_org, digits = 4, decimal.mark = ','))) +
  coord_cartesian(ylim=c(0.320, 0.33), xlim = c(0.25,7.75), expand = F)+
  scale_y_continuous(labels = format(seq(0.32, .33, .0025), digits = 4, decimal.mark = ',')) +
  labs(x = '', y='gini') +
  theme_chart +
  theme(axis.line.x = element_blank(),
        axis.ticks.x = element_blank())
  
size <- dplyr::filter(map_dim, map=='chart')
ggsave("latex/dissertacao_bms_vfull/figuras/03_resultados/02_zeis/gini_scenarios_pop.png", 
       dpi = 500, width = size$width, height = size$height*.9, units = "mm")

# figure 31: lorenz scenarios ----

median_uh <- df_lor %>% 
  dplyr::filter(group!='', class != 'domicílio') %>% 
  dplyr::group_by(group, class, scenario) %>% 
  dplyr::summarise(median.x = weighted.median(cumuh, uh),
                   median.y = weighted.median(cumacc, uh)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(scenario = factor(scenario,
                                  levels = paste('cenário', c('original', 'base', 1:6)))) 
median <- df_lor %>% 
  dplyr::filter(group!='') %>% 
  dplyr::group_by(group, scenario) %>% 
  dplyr::summarise(median.x = weighted.median(cumuh, uh),
                   median.y = weighted.median(cumacc, uh)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(scenario = factor(scenario,
                                  levels = paste('cenário', c('original', 'base', 1:6))),
                class = 'todas') %>% 
  dplyr::bind_rows(median_uh)
  
p <- df_lor %>%
  dplyr::mutate(scenario = factor(scenario,
                                  levels = paste('cenário', 
                                                 c('original', 'base', '',
                                                   1:6)))) %>% 
  ggplot() +
  geom_line(aes(x=cumuh, y=cumacc), 
            lwd = line.size*.008)+
  geom_abline(intercept=0, slope=1, color="grey50", 
              lwd = line.size*.006, lty = 'dashed') +
  geom_point(data = median,
             aes(x = median.x, y = median.y, 
                 color = group, shape = class), size = 3) +
  geom_text(data = gini,
            aes(x = 0.3, y = 0.95,
                label = paste0('Gini: ', format(gini,digits=4,decimal.mark=',')))) +
  scale_color_manual('grupo',
                     breaks = c('g1', 'g2', 'g3'),
                     values = colOrange(5)[c(1,3,5)]) +
  scale_shape_manual('domicílio',
                     breaks = c('todas', 'UH'),
                     labels = c('todos + UHs', 'UHs propostas'),
                     values = 16:17)+
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  labs(x='domicílio', y='acessibilidade',
       caption = 'g1: renda até 3SM; g2: renda entre 3SM a 10SM; g3: renda acima de 10SM') + 
  coord_fixed(xlim=c(0,1), ylim=c(0,1), expand = F)+
  facet_wrap(~ scenario, drop=F)+
  theme_chart +
  theme(panel.spacing = unit(7.5, 'mm'),
        plot.caption.position = 'plot',
        legend.position = c(0.85,0.87))

g <- ggplotGrob(p)
# get the grobs that must be removed
rm_grobs <- g$layout$name %in% c("panel-1-3", "strip-t-3-1")
# remove grobs
g$grobs[rm_grobs] <- NULL
g$layout <- g$layout[!rm_grobs, ]

size <- dplyr::filter(map_dim, map=='chart')
ggsave("latex/dissertacao_bms_vfull/figuras/03_resultados/02_zeis/lorenz_scenarios_pop.png", 
      g, dpi = 500, width = size$width, height = size$width*.8, units = "mm")

# apendix: bar plots ----

df_acc <- purrr::map_df(c(2010,2014), function(y){
  
  a <- acc %>% 
    dplyr::filter(year == y)
  
  brk <- quantile(a$perc, probs = seq(0,1,1/quant))
  
  per <- ifelse(y == 2010, 'stage 1', 'stage 2')
  
  df %>% 
    dplyr::filter(period == per) %>% 
    dplyr::mutate(range = as.numeric(as.character(factor(cut(acc,
                                                             breaks = brk,
                                                             label = (1:quant)-.5,
                                                             include.lowest = T,
                                                             right = F)))))
  
})

p <- df_acc %>% 
  dplyr::mutate(scenario = factor(scenario,
                                  levels = c('base', 1:8)),
                class = factor(class,
                               levels = c('lower', 'medium', 'higher'))) %>% 
  dplyr::group_by(class, range, scenario, period) %>% 
  dplyr::summarise(uh = sum(uh)) %>% 
  ggplot() +
  geom_bar(aes(class, uh, fill = range), position = 'fill', stat='identity') +
  scale_fill_stepsn('percentile',
                    colors = colOrange(quant*2)[seq(2,quant*2,2)],
                    values = scales::rescale((1:quant)-.5, 
                                             from = c(0,quant)),
                    breaks = 0:quant,
                    labels = 0:5,
                    limits = c(0,quant),
                    expand = F,
                    right = T,
                    na.value=colOrange(quant*2)[2],
                    guide = guide_colorbar(order = 2,
                                           nbin = 200,
                                           title.position = "top",
                                           label.position = "bottom",
                                           barheight = base/45*.7,
                                           barwidth = base/3*.7,
                                           default.unit = "mm",
                                           ticks.colour = NA)) +
  scale_y_continuous(labels = scales::percent) +
  coord_flip()+
  labs(y = '% of HU', x = '') +
  facet_grid(scenario ~ period, switch = 'y') +
  theme_chart+
  theme(legend.position = 'bottom',
        strip.placement = 'outside')

p 


