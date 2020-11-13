#' Plots the figures 06 to 12
#' 
#' figure 06: bar plot to justify accessibility to jobs
#' figure 07: map of jobs at sao paulo
#' figure 08: bar plot of distribution of cells by scale of jobs
#' figure 09: map of accessibility at sao paulo
#' figure 10: map of distribution of groups at sao paulo
#' figure 11: map of isolation of low income group
#' figure 12: bar plot of distribution of cells by scale of isolation

# Library ----
library(tidyverse)
library(patchwork)
base <- 210 # width of images
source('rscript/00_Maps.R')
library(RPostgres)
library(DBI)
quant <- 5

# Reading files ----

# population
pop_micro <- readr::read_csv('data/population/population_micro_grid2.csv') 

# local segregation
load('results/02_housing_acc_segreg/table/segregation/localSegreg.rda')
segregation <- lSeg %>% 
  dplyr::filter(measure == 'G1_G1', mtype=='euclidean') %>% 
  dplyr::mutate(bandwd = paste0(trimws(format(bandwd,big.mark = '.')),'m')) 

# global segregation
load('results/02_housing_acc_segreg/table/segregation/globalSegreg.rda')
global <- gSeg %>% 
  dplyr::filter(measure == 'G1_G1', mtype=='euclidean') %>% 
  dplyr::rename(global=value) %>% 
  dplyr::mutate(bandwd = paste0(trimws(format(bandwd,big.mark = '.')),'m')) 
rm(lSeg, gSeg)
gc()

# jobs
opportunities <- sf::st_read('data/opportunities/jobs_grid_nereus.shp', quiet = T) %>% 
  sf::st_transform(prjdegrees) %>% 
  dplyr::rename(job = emprego,
                year = ano) %>% 
  dplyr::filter(year %in% 2010:2017) 

# accessibility
accessibility <- readr::read_csv('results/02_housing_acc_segreg/table/accessibility_distances.csv') %>% 
  dplyr::mutate(threshold = paste0(threshold, 'min')) %>% 
  dplyr::filter(threshold!='5min', year==2017)

# total of jobs
tot_jobs <- opportunities %>% 
  dplyr::filter(year==2017) %>% 
  dplyr::summarise(sum = sum(job, na.rm=T)) %>% 
  .$sum

grid_nereus <- sf::st_read('data/shp/grid_nereus.shp', quiet = T) %>% 
  sf::st_transform(prjdegrees)

od17 <- foreign::read.dbf('data/transportation/od/OD_2017.dbf')

# ACCESSIBILITY DECISIONS ----

# figure 06: bar plot to justify accessibility to jobs ----

p <- od17 %>% 
  dplyr::filter(SERVIR_D == 2, MUNI_DOM == 36) %>% 
  dplyr::group_by(MOTIVO_D) %>% 
  dplyr::summarise(viagens = sum(FE_VIA, na.rm=T)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(motivo = factor(MOTIVO_D,
                                labels = c(rep('trabalho',  3), 
                                           'escola', 'compras', 'saúde',
                                           'lazer', 'residência', 'procurar\nemprego',
                                           'assuntos\npessoais', 'refeição'))) %>% 
  dplyr::filter(motivo!='residência') %>% 
  dplyr::group_by(motivo) %>% 
  dplyr::summarise(viagens = sum(viagens, na.rm=T)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(motivo = factor(motivo,
                                levels = c('trabalho', 'escola', 'assuntos\npessoais', 
                                           'compras', 'lazer', 'saúde', 'refeição', 
                                           'procurar\nemprego'))) %>% 
  ggplot() +
  geom_bar(aes(y = viagens/sum(viagens), x = motivo), stat='identity', fill = colOrange(5)[3]) +
  geom_text(aes(y = viagens/sum(viagens)+0.02, 
                label = scales::percent(round(viagens/sum(viagens), 3), decimal.mark = ','), x = motivo)) + 
  scale_y_continuous(labels = scales::percent_format(decimal.mark = ',')) +
  coord_cartesian(ylim = c(0,0.55), expand = F) +
  labs(x = 'motivo', y = '% das viagens') +
  theme_chart +
  theme(panel.grid.major.x = element_blank())

p

size <- dplyr::filter(map_dim, map=='chart')
ggsave('latex/dissertacao_bms_vfull/figuras/design/barplot_od_jobs.png',
       dpi = 500, type = 'cairo-png', width = size$width, height = size$height, units = 'mm')


# figure 07: map of jobs at sao paulo ----

max <- max(opportunities$job, na.rm=T)
max <- dplyr::filter(opportunities, job == max) %>%  .$year

brks <- opportunities %>% 
  dplyr::filter(year == max, job!=0) %>% 
  .$job %>% 
  quantile(., probs = seq(0,1,1/quant))

brks[1] <- 0

plot <- opportunities %>% 
  dplyr::mutate(range = cut(job, 
                            breaks = brks,
                            include.lowest = T,
                            right = F)) %>% 
  dplyr::group_by(range, year) %>% 
  dplyr::summarise() %>% 
  dplyr::ungroup()

p_jobs <- ggbase_brk(plot, 'empregos (10³)') +
  theme_map_3

size <- dplyr::filter(map_dim, map=='4')
ggsave(paste0('latex/dissertacao_bms_vfull/figuras/design/map_jobs.png'), 
      p_jobs, dpi = 500, width = size$width, height = size$height, units = "mm", type = 'cairo-png')

# figure 08: bar plot of distribution of cells by scale of jobs ----

labs <- as.numeric(stringr::str_replace_all(levels(plot$range), '\\[|\\)|\\]|.*,',''))
labs <- c(0, formatC(as.numeric(labs[1:4]), format='d',big.mark = '.', decimal.mark = ','), expression(''%->%''))

opportunities %>% 
  dplyr::filter(job != 0 ) %>% 
  dplyr::mutate(range = as.numeric(cut(job, 
                                       breaks = brks,
                                       include.lowest = T,
                                       right = F))-.5,
                year = factor(year,
                              levels = as.character(c(2017, 2014, 2010, 2007, 2002)))) %>% 
  dplyr::group_by(range, year) %>% 
  dplyr::summarise(ct = dplyr::n()) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(year) %>% 
  dplyr::mutate(perc = ct/sum(ct, na.rm=T))

p <- opportunities %>% 
  dplyr::filter(job != 0 ) %>% 
  dplyr::mutate(range = as.numeric(cut(job, 
                                       breaks = brks,
                                       include.lowest = T,
                                       right = F))-.5,
                year = factor(year,
                              levels = as.character(c(2017, 2014, 2010, 2007, 2002)))) %>% 
  dplyr::group_by(range, year) %>% 
  dplyr::summarise(ct = dplyr::n()) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(year) %>% 
  dplyr::mutate(perc = ct/sum(ct, na.rm=T)) %>% 
  ggplot() + 
  geom_bar(aes(x = year, y = perc, fill = range), position = 'stack', stat = 'identity') +
  scale_fill_stepsn('empregos (10³)',
                    colors = colOrange(quant),
                    values = scales::rescale((1:quant)-.5, 
                                             from = c(0,quant)),
                    breaks = 0:quant,
                    labels = labs,
                    limits = c(0,quant),
                    na.value=colOrange(10)[2],
                    guide = guide_colorbar(order = 2,
                                           nbin = 200,
                                           title.position = "top",
                                           label.position = "bottom",
                                           barheight = base/45*.7,
                                           barwidth = base/3*.7,
                                           default.unit = "mm",
                                           ticks.colour = NA,
                                           override.aes = list(shape = NA,
                                                               linetype = "blank")),
                    aesthetics = "fill") +
  scale_y_continuous(labels = scales::percent, breaks = seq(0,1,.2))+
  coord_flip(expand = F) +
  labs(y= 'porcentagem', x = 'ano') +
  theme_chart +
  theme(plot.margin = margin(2,7.5,2,2,'mm'))

p

size <- dplyr::filter(map_dim, map=='chart')
ggsave(paste0('latex/dissertacao_bms_vfull/figuras/design/barplot_jobs.png'), 
       p, dpi = 500, width = size$width, height = size$height, units = "mm", type = 'cairo-png')

# division of trips to jobs by mode----

df <- od17 %>% 
  dplyr::filter(SERVIR_D == 2) %>% 
  dplyr::group_by(MOTIVO_D, MODOPRIN) %>% 
  dplyr::summarise(viagens = sum(FE_VIA, na.rm=T)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(motivo = factor(MOTIVO_D,
                                labels = c(rep('Trabalho',  3), 
                                           'Escola', 'Compras', 'Saúde',
                                           'Lazer', 'Residência', 'Procurar\nEmprego',
                                           'Assuntos\nPessoais', 'Refeição'))) %>% 
  dplyr::filter(motivo=='Trabalho') %>% 
  dplyr::mutate(modo = factor(MODOPRIN,
                              levels = 1:17,
                              labels = c(rep('TP', 6),
                                         'fretado', 'escolar',
                                         rep('ind', 6),
                                         rep('atv', 2),
                                         'outros'))) %>% 
  dplyr::group_by(modo) %>% 
  dplyr::summarise(viagens = sum(viagens, na.rm = T)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(perc = viagens / sum(viagens))

df

# threshold ----
thr <- od17 %>% 
  dplyr::filter(MOTIVO_D %in% 1:3, SERVIR_D==2, MODOPRIN %in% 1:6, MUNI_DOM == 36)

thr <- spatstat::weighted.median(thr$DURACAO, thr$FE_VIA)

# figure 09: map of accessibility at sao paulo ----

# joining grid
df_acc <- accessibility %>% 
  dplyr::mutate(perc = acc/tot_jobs,
                range = as.numeric(as.character(factor(cut(perc,
                                                           breaks = seq(0,1,1/quant),
                                                           label = (1:quant)-.5,
                                                           include.lowest = T,
                                                           right = F))))) %>% 
  dplyr::rename(id = origin) %>% 
  dplyr::right_join(sf::st_drop_geometry(grid)) %>% 
  tidyr::complete(id, fill=list(range = 0.5, perc = 0, acc = 0)) %>% 
  dplyr::filter(!is.na(threshold)) %>% 
  dplyr::mutate(threshold = factor(threshold,
                                   levels = c('15min', '30min', '60min', '120min')))

plot_acc <- df_acc %>% 
  dplyr::left_join(grid) %>% 
  sf::st_as_sf() %>% 
  dplyr::group_by(threshold, range) %>% 
  dplyr::summarise() %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(threshold = factor(trimws(threshold), 
                                   levels =c('15min', '30min', '60min', '120min')))
p <- plot_acc %>% 
  ggbase_perc(., '% do total de empregos') +
  facet_grid(. ~ threshold) 

size <- dplyr::filter(map_dim, map=='4')
ggsave(paste0('latex/dissertacao_bms_vfull/figuras/design/map_acc.png'), 
       p, dpi = 500, width = size$width, height = size$height, units = "mm", type='cairo-png')

# SEGREGATION DECISIONS ----
# figure 10: map of distribution of groups at sao paulo ----

pop_micro[is.na(pop_micro)] <- 0

pop <- pop_micro %>% 
  dplyr::rename(id = id_grid) %>% 
  dplyr::mutate(tot = g1+g2+g3,
                G1 = g1/tot,
                G2 = g2/tot,
                G3 = g3/tot) %>% 
  dplyr::select(id, G1, G2, G3)

plot <- pop %>%
  tidyr::gather(group, perc, G1:G3) %>% 
  dplyr::mutate(range = as.numeric(cut(perc,
                                      breaks = seq(0,1,0.2),
                                      right = F,
                                      include.lowest = T)) - 0.5) %>% 
  dplyr::left_join(grid) %>% 
  sf::st_as_sf() %>% 
  dplyr::group_by(group, range) %>% 
  dplyr::summarise() %>% 
  dplyr::ungroup()

p <- ggbase_perc(plot, '% do grupo dentro da célula') +
  facet_grid(.~group) +
  theme_map_3

size <- dplyr::filter(map_dim, map=='4')
ggsave(paste0('latex/dissertacao_bms_vfull/figuras/design/map_pop_micro_perc.png'), 
       p, dpi = 500, width = size$width, height = size$height, units = "mm", type = 'cairo-png')

# figure 11: map of isolation of low income group  ----

segregation <- segregation %>% 
  dplyr::filter(measure == 'G1_G1') %>% 
  dplyr::mutate(logvalue = log10(value))

max <- segregation %>% 
  dplyr::filter(!is.infinite(value)) %>% 
  dplyr::summarise(max= max(value, na.rm=T)) %>% 
  dplyr::ungroup() %>% 
  dplyr::left_join(segregation, by=c('max'='value')) %>% 
  dplyr::distinct(max, .keep_all=T)

brk <- segregation %>% 
  dplyr::filter(bandwd == max$bandwd, mtype == max$mtype) %>% 
  .$value %>% 
  quantile(.,seq(0,1,1/quant), names=F)

df_seg <- segregation %>% 
  dplyr::mutate(range = as.numeric(as.character(cut(value, 
                                                    breaks = brk,
                                                    labels = (1:quant)-0.5,
                                                    include.lowest = T,
                                                    right = T)))) %>% 
  dplyr::right_join(sf::st_drop_geometry(grid))  %>% 
  tidyr::complete(id, tidyr::nesting(mtype, bandwd), fill=list(range = 0.5)) %>% 
  dplyr::filter(!is.na(mtype), !is.na(bandwd)) %>% 
  dplyr::mutate(bandwd = factor(bandwd,
                                levels = c('500m', '1.500m', '5.000m', '10.000m')))

plot_seg <- df_seg %>% 
  dplyr::left_join(grid) %>% 
  sf::st_as_sf() %>% 
  dplyr::group_by(mtype, bandwd, range) %>% 
  dplyr::summarise() %>%
  dplyr::ungroup() %>% 
  dplyr::left_join(global) %>% 
  dplyr::mutate(bandwd = factor(trimws(as.character(bandwd)),
                                levels = c('500m', '1.500m', '5.000m', '10.000m'))) %>%     
  dplyr::select(mtype, bandwd, range, global)

p <- plot_seg %>% 
  ggbase_isolation(., 'isolamento local (quantil)', matrix_type = max$mtype, bandwidth = max$bandwd) +
  facet_grid(. ~ bandwd) 

size <- dplyr::filter(map_dim, map=='4')
ggsave(paste0('latex/dissertacao_bms_vfull/figuras/design/map_G1_G1_micro.png'), 
       p, dpi = 500, type = 'cairo-png', width = size$width, height = size$height, units = "mm")

# figure 12: bar plot of distribution of cells by scale of isolation ----

p <- df_seg %>% 
  dplyr::mutate(bandwd = factor(bandwd,
                                levels = c('10.000m', '5.000m', '1.500m', '500m'))) %>% 
  dplyr::group_by(range, bandwd) %>% 
  dplyr::summarise(ct = dplyr::n()) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(bandwd) %>% 
  dplyr::mutate(perc = ct/sum(ct, na.rm=T)) %>% 
  ggplot() + 
  geom_bar(aes(x = bandwd, y = perc, fill = range), position = 'stack', stat = 'identity')+
  scale_fill_stepsn('isolamento local (quantil)',
                    colors = colOrange(quant),
                    values = scales::rescale((1:quant)-.5, 
                                             from = c(0,quant)),
                    breaks = 0:quant,
                    labels = c(expression(''%<-%''), rep('',quant-1), expression(''%->%'')),
                    limits = c(0,quant),
                    na.value=colOrange(quant),
                    guide = guide_colorbar(order = 2,
                                           nbin = 200,
                                           title.position = "top",
                                           label.position = "bottom",
                                           barheight = base/45*.7,
                                           barwidth = base/3*.7,
                                           default.unit = "mm",
                                           ticks.colour = NA,
                                           override.aes = list(shape = NA,
                                                               linetype = "blank")),
                    aesthetics = "fill") +
  scale_y_continuous(labels = scales::percent, breaks = seq(0,1,.2))+
  coord_flip(expand = F) +
  labs(y= 'porcentagem', x = 'banda') +
  theme_chart +
  theme(plot.margin = margin(2,7.5,2,2,'mm'))


size <- dplyr::filter(map_dim, map=='chart')
ggsave(paste0('latex/dissertacao_bms_vfull/figuras/design/barplot_segreg.png'), 
       p, dpi = 500, width = size$width, height = size$height, units = "mm", type = 'cairo-png')
