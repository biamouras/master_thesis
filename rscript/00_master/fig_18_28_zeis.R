#' Plots figures 18-28

# Library ----

library(tidyverse)
library(ggnewscale)
library(treemapify)
library(ggnomics)
library(gtable)
library(grid)
library(spatstat)
base <- 210
quant <-  5
source('rscript/00_Maps.R')

# Reading files----

# accessibility
acc_org <- readr::read_csv('results/02_housing_acc_segreg/table/accessibility_distances.csv') 

acc <- acc_org %>% 
  dplyr::mutate(threshold = paste0(threshold, 'min')) %>% 
  dplyr::filter(threshold=='60min', year %in% c(2010, 2014))

acc_aux <- acc %>% 
  dplyr::rename(id = origin) %>% 
  dplyr::select(id, year, perc) %>% 
  tidyr::pivot_wider(names_from = year, names_prefix = 'y_', values_from = 'perc')

# population
pop_micro <- readr::read_csv('data/population/population_micro_grid2.csv') 

# housing
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
                                         TRUE ~ 'alto padrão'),
                period = stringr::str_replace_all(period, 'Stage', 'fase'),
                origin = 'privado') %>% 
  dplyr::select(id, period, class, uh, origin)

isobands <- sf::st_read('data/housing/private/private_isobands.shp', quiet = T) %>% 
  dplyr::mutate(period = stringr::str_replace_all(period, 'Stage', 'fase'))

private_df <- private_org %>% 
  sf::st_drop_geometry()

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

housing <- rbind(mcmv, private) %>%
  dplyr::left_join(acc_aux) %>% 
  dplyr::mutate(acc = dplyr::case_when(period == 'fase 1' ~ y_2010,
                                       TRUE ~ y_2014)) %>% 
  dplyr::select(-y_2010, -y_2014)

# zeis
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

scenarios <- readr::read_csv('results/03_zeis/table/scenarios.csv')

trilho_full <- trilhos

trilhos <- trilhos %>%
  dplyr::filter(ano %in% c(2010, 2014)) %>% 
  dplyr::mutate(period = ifelse(ano == 2010,'fase 1', 'fase 2'))

# PRIVATE ----

# figure 18: boxplot price per area ----

private_df %>% 
  dplyr::select(ANO_LAN, PC_AU_ATU, AR_UT_UNID) %>%
  dplyr::rename(ano = ANO_LAN,
                preco_au = PC_AU_ATU,
                au = AR_UT_UNID) %>% 
  tidyr::pivot_longer(c(preco_au, au), names_to = 'var', values_to = 'value') %>% 
  ggplot(aes(x=ano, y=value)) +
  geom_boxplot(color = 'grey40', fill = 'transparent', alpha = 0.3, lwd = line.size/200)+
  scale_y_continuous(labels = scales::label_number(big.mark = '.', decimal.mark = '.')) +
  facet_wrap(~var, 
             scales = 'free_y',
             strip.position = 'left',
             labeller = as_labeller(c(au = 'área útil (m²)',
                                      preco_au = 'preço por área útil (R$/m²)')))+
  labs(x = 'ano', y=NULL)+
  theme_chart +
  theme(strip.placement = 'outside')

size <- dplyr::filter(map_dim, map=='chart')
ggsave('latex/dissertacao_bms_vfull/figuras/resultados/zeis/boxplot_price_area.png',
       dpi = 500, type = 'cairo-png', width = size$width, height = size$height, units = 'mm')

# figure 19: area - classes HIS, HMP and high ----

private_df %>%
  dplyr::group_by(ANO_LAN, classe) %>%
  dplyr::summarize(qtde = sum(TT_UNID)) %>% 
  dplyr::ungroup() %>%
  dplyr::group_by(ANO_LAN) %>% 
  dplyr::mutate(prop = qtde/ sum(qtde, na.rm=T)) %>% 
  ggplot() +
  geom_area(aes(x = ANO_LAN, y = prop, 
                fill = classe, group = classe), 
            stat = 'identity', position = 'stack')  +
  scale_fill_manual('class',
                    breaks = c('lower',
                               'medium',
                               'higher'),
                    values = colOrange(5)[c(1,3,5)],
                    labels = c('HIS', 
                               'HMP',
                               'alta renda'),
                    drop = F) +
  scale_y_continuous(labels = scales::percent)+
  labs(y = '% do total de UH',
       x = 'ano de lançamento') +
  coord_cartesian(ylim =  c(0,1), expand = F) +
  theme_chart +
  theme(plot.margin = margin(2,7.5,2,2,'mm')) 

size <- dplyr::filter(map_dim, map=='chart')
ggsave('latex/dissertacao_bms_vfull/figuras/03_resultados/02_zeis/area_types.png',
       dpi = 500, type = 'cairo-png', width = size$width, height = size$height, units = 'mm')

# figure 20: heatmap location by type ----

# * curves df ----
curves <- data.frame(text = c('Pq. do\nCarmo', 'Jacu-Pêssego', 'Linha\nLilás', 
                              'Água\nBranca','Vila\nAndrade', 'Tatuapé',
                              'Sacomã', 'Vila\nAndrade', 'Brooklin',
                              'Santa\nCruz'),
                     period = c(rep('fase 1', 2), 'fase 2', 
                                rep('fase 1', 4), 
                                rep('fase 2', 3)),
                     class = c(rep('HMP', 3), 
                               rep('alto padrão', 7)),
                     curve = c(-1, -1, 1,
                               1, 1, -1,
                               1, 1, -1,
                               1)*0.4,
                     xend = c(-46.4822721,-46.4550387,-46.7470963,
                              -46.6811997,-46.7364537, -46.5787827,
                              -46.6048974,-46.7364537,-46.6890608,
                              -46.6260117),
                     x = c(-46.4597257,-46.4752317,-46.7563276,
                           -46.7056897, -46.7563276, -46.4822721,
                           -46.5606937, -46.7563276, -46.6763276,
                           -46.5606937),
                     yend = c(-23.5886771, -23.535775, -23.651803, 
                              -23.517864, -23.625576, -23.5402718,
                              -23.6029751, -23.625576, -23.6196256,
                              -23.599748),
                     y = c(-23.642384, -23.4305731, -23.6885278, 
                           -23.46232, -23.6885278, -23.5886771,
                           -23.659926, -23.6885278, -23.6885278,
                           -23.659926),
                     hjust = c(.04, 0, -.03,
                               .03, -.02, .04,
                               0, -.02, .04,
                               0),
                     vjust = c(-.01, .02, 0,
                               .03, -.01, 0,
                               -.02, -.01, -.01,
                               -.02),
                     angle = c(90, 60, 90,
                               120, 100, 60,
                               120, 100, 100,
                               120))

# * base map ----
p <- isobands %>% 
  dplyr::mutate(class = factor(class,
                               levels = c('medium', 'higher'),
                               labels = c('HMP', 'alto padrão')),
                period = stringr::str_replace(period, 'stage', 'fase')) %>% 
  ggplot() +
  annotation_custom(ggbasemap_short, xmin=-Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
  geom_sf(data = massa, fill = colWater, color = colWater, size = line.size/500)  +
  geom_sf(data=rios, color=colWater, size=line.size/250) + 
  geom_sf(data=verde, fill=colGreen, color='transparent') +
  geom_sf(data=muni, fill = 'grey40', color = 'transparent', alpha = 0.25)+
  # heatmap
  geom_sf(aes(fill = level), alpha = 0.6, color = NA)+
  scale_fill_stepsn('# de UH',
                    colors = colOrange(5),
                    values = scales::rescale((1:5)-.5, 
                                             from = c(0,5)),
                    breaks = (0:5)*10,
                    labels = (0:5)*10,
                    limits = c(0, 50),
                    expand = F,
                    right = T,
                    guide = guide_colorsteps(order = 2,
                                             even.steps = F,
                                             title.position = "top",
                                             barheight = base/45,
                                             barwidth = base/3,
                                             default.unit = "mm",
                                             ticks.colour = NA,
                                             override.aes = list(shape = NA,
                                                                 linetype = "blank")))+
  new_scale_fill()+
  geom_sf(data=trilhos, aes(fill = 'trilhos'), color = 'grey40',
          size = line.size/200, show.legend = 'line') +
  labs(caption = paste0('fase 1: 2009-2010; fase 2: 2011-2013\n',
                        bg$caption))  +
  scale_fill_manual(breaks = c('trilhos'),
                    values = c('trilhos' = 'grey40'),
                    labels = c('trilhos'),
                    guide = guide_legend('',
                                         order = 1,
                                         title.position = 'top',
                                         override.aes = list(shape = NA)))+
  ggspatial::annotation_scale(location = 'br', 
                              bar_cols = c('grey15', 'grey99'), 
                              line_width = 0.3,
                              line_col = 'grey15',
                              text_col = 'grey15') +
  facet_grid(class ~ period, switch = 'y')+
  ggplot2::theme_bw()+
  theme_map +
  coord_lim_short

# * adding annotation ----
curves %>% 
  purrr::pwalk(function(...){
    
    # gets all the values
    current <- dplyr::tibble(...)
    
    # update the map globally
    p <<- p +
      geom_curve(data = current, 
                 aes(x = x, xend = xend, y = y, yend = yend),
                 curvature = current %>% pull(curve),
                 angle = current %>% pull(angle),
                 size = line.size/200,
                 arrow = arrow(length = unit(line.size/50, 'mm'))) +
      geom_text(data = current, 
                aes(x = x+hjust, y = y+vjust, label = text), size = font.size*0.035)
    
    
  })

size <- dplyr::filter(map_dim, map=='2')
ggsave(paste0('latex/dissertacao_bms_vfull/figuras/resultados/zeis/map_private_heatmap.png'), 
       p, dpi = 500, type = 'cairo-png', width = size$width, height = size$height+5, units = 'mm')  

# figure 21: accessibility - relative scale ----

# treating data
df_acc <- acc %>% 
  dplyr::mutate(range = as.numeric(as.character(factor(cut(perc,
                                                           breaks = seq(0,1,1/quant),
                                                           label = (1:quant)-.5,
                                                           include.lowest = T,
                                                           right = F))))) %>% 
  dplyr::rename(id = origin) %>% 
  dplyr::right_join(sf::st_drop_geometry(grid)) %>% 
  tidyr::complete(id, fill=list(range = 0.5, perc = 0, acc = 0)) %>% 
  dplyr::filter(!is.na(threshold))

# data for acc maps
plot_acc <- df_acc %>% 
  dplyr::left_join(grid) %>% 
  sf::st_as_sf() %>% 
  dplyr::group_by(year, range) %>% 
  dplyr::summarise() %>% 
  dplyr::ungroup() 

# data for difference map
dif <- df_acc %>% 
  dplyr::select(id, year, perc) %>% 
  tidyr::pivot_wider(names_from = year, names_prefix = 'y_', values_from = perc) %>% 
  dplyr::mutate(dif = y_2014/y_2010 - 1,
                diff = as.numeric(as.character(cut(dif,
                                                   breaks = c(-Inf, -0.5, -.1,.1,.5,Inf),
                                                   labels= 1:5))),
                year = 'diferença') %>% 
  dplyr::left_join(grid) %>% 
  sf::st_as_sf() %>% 
  dplyr::group_by(year, diff) %>% 
  dplyr::summarise() %>% 
  dplyr::ungroup() 

breaks <- c(-Inf, -0.5, -.1,.1,.5,Inf)
lb <- c(expression(''%<-%''), scales::percent(breaks[c(-1,-(quant+1))]), expression(''%->%''))

p <- ggplot() +
  annotation_custom(ggbasemap, xmin=-Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
  geom_sf(data = plot_acc, aes(fill = range), color = NA) +
  scale_fill_stepsn('% do total de empregos',
                    colors = colOrange(quant),
                    values = scales::rescale((1:quant)-.5, 
                                             from = c(0,quant)),
                    breaks = 0:quant,
                    labels = seq(0,100,100/quant),
                    limits = c(0,quant),
                    expand = F,
                    right = T,
                    na.value=colOrange(quant)[1],
                    guide = guide_colorbar(order = 2,
                                           nbin = 200,
                                           title.position = "top",
                                           label.position = "bottom",
                                           barheight = base/45*.7,
                                           barwidth = base/3*.7,
                                           default.unit = "mm",
                                           ticks.colour = NA,
                                           override.aes = list(shape = NA,
                                                               linetype = "blank"))) +
  new_scale_fill()+
  geom_sf(data = dif, aes(fill = diff), color = NA) +
  scale_fill_stepsn('diferença',
                    colors = colRedBlue(quant),
                    breaks = 0:quant,
                    values = scales::rescale((1:quant)-.5, 
                                             from = c(0,quant)),
                    labels = lb,
                    limits = c(0,quant),
                    expand = F,
                    right = T,
                    na.value=colRedBlue(3),
                    guide = guide_colorbar(order = 3,
                                           nbin = 200,
                                           title.position = "top",
                                           label.position = "bottom",
                                           barheight = base/45*.7,
                                           barwidth = base/3*.7,
                                           default.unit = "mm",
                                           ticks.colour = NA,
                                           override.aes = list(shape = NA,
                                                               linetype = "blank")))+
  geom_sf(data = massa, fill = colWater, color = colWater, size = line.size/500)  +
  geom_sf(data=rios, color=colWater, size=line.size/250) + 
  geom_sf(data=verde, fill=colGreen, color="transparent") +
  geom_sf(data=muni, fill = 'grey40', color = 'transparent', alpha = 0.25) +
  geom_sf(data=trilhos, aes(color = "trilhos"),
          size = line.size/200, show.legend = "line") +
  labs(caption = bg$caption) +
  scale_color_manual(breaks = "trilhos",
                     values = c("trilhos" = "grey40"),
                     guide = guide_legend(title= "", 
                                          order = 1,
                                          title.position = "top")) +
  ggspatial::annotation_scale(location = 'br', 
                              bar_cols = c('grey15', 'grey99'), 
                              line_width = 0.3,
                              line_col = 'grey15',
                              text_col = 'grey15') +
  theme_bw() +
  theme_map_3 +
  coord_lim +
  facet_grid(. ~ year) 

size <- dplyr::filter(map_dim, map=='4')
ggsave(paste0('latex/dissertacao_bms_vfull/figuras/resultados/zeis/map_acc_2010_2014.png'), 
       p, dpi = 500, width = size$width, height = size$height, units = "mm", type='cairo-png')

# figure 22: boxplot accessibility housing ----

housing %>% 
  dplyr::mutate(class = factor(class,
                               levels = c('HIS', 'HMP', 'alto padrão'),
                               labels = c('HIS', 'HMP', 'alto padrão'))) %>% 
  ggplot()+
  geom_boxplot(aes(x = origin, y = acc, fill = class), 
               alpha = 0.3, color = 'grey40', lwd = line.size*.005) +
  scale_fill_manual('classe',
                    breaks = c('HIS', 'HMP', 'alto padrão'),
                    values = colOrange(5)[c(1,3,5)]) +
  scale_y_continuous(labels = scales::percent, limits = c(0,1), expand = c(0,0)) +
  labs(x='', y='% do total de empregos',
       caption = 'fase 1: 2009-2010; fase 2: 2011-2014') + 
  facet_grid(~period) +
  theme_chart 

size <- dplyr::filter(map_dim, map=='chart')
ggsave(paste0('latex/dissertacao_bms_vfull/figuras/resultados/zeis/boxplot_housing_acc.png'),
       dpi = 500, width = size$width, height = size$height, units = "mm")

# figure 23: Lorenz accessibility of housing----

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

p <- df_lor %>% 
  dplyr::mutate(class = factor(class,
                               levels = c('lower', 'medium', 'higher'))) %>% 
  ggplot() +
  geom_line(aes(x=cumuh, y=cumacc), 
            lwd = line.size*.01)+
  geom_abline(intercept=0, slope=1, color="grey50", 
              lwd = line.size*.008, lty = 'dashed') +
  geom_point(data = median, aes(x = median.x, y = median.y, color = class, shape = origin), size = 4) +
  geom_text(data = gini_base, aes(x = 0.2, y = 0.95, 
                                  label = paste0('Gini: ', format(gini,digits=4,decimal.mark=',')))) +
  scale_color_manual('classe', 
                     breaks = c('HIS', 'HMP', 'alto padrão'),
                     values = colOrange(5)[c(1,3,5)]) +
  scale_shape_manual('origem',
                     breaks = c('mcmv', 'privado'),
                     values = 16:17)+
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  labs(x='UH', y='acessibilidade',
       caption = 'fase 1: 2009-2010; fase 2: 2011-2014') + 
  coord_fixed(xlim=c(0,1), ylim=c(0,1), expand = F)+
  facet_grid(.~period)+
  theme_chart +
  theme(panel.spacing = unit(10, 'mm'),
        plot.margin = margin(
          2, 7.5, 2, 2, 'mm'))

p
size <- dplyr::filter(map_dim, map=='chart')
ggsave("latex/dissertacao_bms_vfull/figuras/resultados/zeis/lorenz_acc_uh.png", 
       p, dpi = 500, width = size$width*.7, height = size$height, units = "mm")

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
ggsave('latex/dissertacao_bms_vfull/figuras/resultados/zeis/tree_zeis_used.png',
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
  dplyr::filter(class != 'alto padrão') %>% 
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
ggsave('latex/dissertacao_bms_vfull/figuras/resultados/zeis/boxplot_zeis_used.png',
       dpi = 500, type = 'cairo-png', width = size$width, height = size$height, units = 'mm')

# SCENARIOS ----
# scenarios
scenes <- dplyr::tibble(sc = c('base', 1:6),
                        nhis = c('-', rep(c('max', 'min'), each = 2), rep('min', 2)),
                        nhmp = c(rep('-', 3), rep('max', 2), rep('min', 2)),
                        nca = c('-', rep(c('basic', 'max'), 3)))

# * uh by zeis and scenarios ----

scenarios %>% 
  dplyr::group_by(scenario, zeis, class, period) %>% 
  dplyr::summarise(uh = sum(uh)) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(scenario, class) %>% 
  dplyr::mutate(puh = uh/sum(uh)) %>% 
  print(., n=48)

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
                                levels = c('HIS', 'HMP', 'alto padrão'))) %>% 
  ggplot() +
  geom_boxplot(aes(`cenário`, acc, fill = classe), 
               alpha = 0.3, color = 'grey40', lwd = line.size*.005) +
  scale_fill_manual('classe',
                    breaks = c('HIS', 'HMP', 'alto padrão'),
                    values = colOrange(5)[c(1,3,5)]) +
  scale_y_continuous(labels = scales::percent, limits = c(0,1), expand = c(0,0)) +
  labs(y='% total de empregos',
       caption = 'fase 1: 2009-2010; fase 2: 2011-2014') + 
  facet_grid(~period) +
  theme_chart +
  theme(axis.ticks.x = element_blank())

size <- dplyr::filter(map_dim, map=='chart')
ggsave(paste0('latex/dissertacao_bms_vfull/figuras/resultados/zeis/boxplot_scenarios.png'),
       dpi = 500, width = size$width, height = size$height, units = "mm")

# figure 28: gini ----

sc <- scenes %>% 
  dplyr::mutate(nca = paste0(nca, ' coef.'),
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
ggsave("latex/dissertacao_bms_vfull/figuras/resultados/zeis/gini_scenarios.png", 
       p, dpi = 500, width = size$width, height = size$height, units = "mm")

# figure 29: lorenz scenarios ----

p <- df_lor %>% 
  dplyr::mutate(class = factor(class,
                               levels = c('HIS', 'HMP', 'alto padrão'))) %>% 
  ggplot() +
  geom_line(aes(x=cumuh, y=cumacc), 
            lwd = line.size*.008)+
  geom_abline(intercept=0, slope=1, color="grey50", 
              lwd = line.size*.006, lty = 'dashed') +
  geom_point(data = median, aes(x = median.x, y = median.y, color = class), size = 3) +
  geom_text(data = gini, aes(x = 0.3, y = 0.95, 
                             label = paste0('Gini: ', format(gini,digits=4,decimal.mark=',')))) +
  scale_color_manual('classe',
                     breaks = c('HIS', 'HMP', 'alto padrão'),
                     values = colOrange(5)[c(1,3,5)]) +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  labs(x='UH', y='acessibilidade',
       caption = 'fase 1: 2009-2010; fase 2: 2011-2014') + 
  coord_fixed(xlim=c(0,1), ylim=c(0,1), expand = F)+
  facet_nested(nhmp + nhis ~ nca + period)+
  theme_chart +
  theme(panel.spacing = unit(7.5, 'mm'),
        plot.caption.position = 'plot')
#bs + 
p
size <- dplyr::filter(map_dim, map=='chart')
ggsave("latex/dissertacao_bms_vfull/figuras/resultados/zeis/lorenz_scenarios.png", 
       p, dpi = 500, width = size$width, height = size$width*.8, units = "mm")

# lorenz of municipality ----

# * preparing data ----

sc <- scenes %>% 
  dplyr::mutate(nca = paste0(nca, ' coef.'),
                nhis = factor(nhis,
                              levels = c('max', 'min'),
                              labels = c('máx. HIS', 'min. HIS')),
                nhmp = factor(nhmp,
                              levels = c('-', 'max', 'min'),
                              labels = c('-', 'máx. HMP', 'min. HMP')))

pop <- merge(pop_micro, data.frame(scenario = paste('cenário', c(1:6, 'base', 'original')))) %>% 
  dplyr::rename(id = id_grid) %>% 
  tidyr::pivot_longer(g1:g3, 'group', values_to = 'uh') %>% 
  dplyr::mutate(class = 'família')

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
ggsave("latex/dissertacao_bms_vfull/figuras/resultados/zeis/gini_scenarios_pop.png", 
       dpi = 500, width = size$width, height = size$height, units = "mm")

# figure 31: lorenz scenarios ----

median_uh <- df_lor %>% 
  dplyr::filter(group!='', class != 'família') %>% 
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
            aes(x = 0.25, y = 0.95,
                label = paste0('Gini: ', format(gini,digits=4,decimal.mark=',')))) +
  scale_color_manual('grupo',
                     breaks = c('g1', 'g2', 'g3'),
                     values = colOrange(5)[c(1,3,5)]) +
  scale_shape_manual('família',
                     breaks = c('todas', 'UH'),
                     labels = c('todas + UH', 'UH propostas'),
                     values = 16:17)+
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  labs(x='família', y='acessibilidade',
       caption = 'g1: renda até 3SM; g2: renda entre 3SM a 10SM; g3: renda acima de 10SM') + 
  coord_fixed(xlim=c(0,1), ylim=c(0,1), expand = F)+
  facet_wrap(~ scenario, drop=F)+
  theme_chart +
  theme(panel.spacing = unit(7.5, 'mm'),
        plot.caption.position = 'plot',
        legend.position = c(0.85,0.85))

g <- ggplotGrob(p)
# get the grobs that must be removed
rm_grobs <- g$layout$name %in% c("panel-1-3", "strip-t-3-1")
# remove grobs
g$grobs[rm_grobs] <- NULL
g$layout <- g$layout[!rm_grobs, ]

size <- dplyr::filter(map_dim, map=='chart')
ggsave("latex/dissertacao_bms_vfull/figuras/resultados/zeis/lorenz_scenarios_pop.png", 
      g, dpi = 500, width = size$width, height = size$width, units = "mm")

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


