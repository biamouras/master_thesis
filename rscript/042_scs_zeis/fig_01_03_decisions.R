# Library ----
library(tidyverse)
library(patchwork)
library(ggnewscale)
base <- 210 # width of images
quant <- 5
source('rscript/00_Maps_eng.R')

# Reading files ----
# od
od17 <- foreign::read.dbf('data/transportation/od/OD_2017.dbf')

# jobs
opportunities <- sf::st_read('data/opportunities/jobs_grid_nereus.shp', quiet = T) %>% 
  sf::st_transform(prjdegrees) %>% 
  dplyr::rename(job = emprego,
                year = ano) %>% 
  dplyr::filter(year == 2017) 

# population 
pop_micro <- readr::read_csv('data/population/population_micro_grid2.csv') 

# segregation 
# local segregation
load('results/02_housing_acc_segreg/table/segregation/localSegreg.rda')
segregation <- lSeg %>% 
  dplyr::filter(measure == 'G3_G3', mtype=='euclidean') %>% 
  dplyr::mutate(bandwd = paste0(trimws(format(bandwd,big.mark = ',',
                                              decimal.mark = '.')),'m')) 

load('results/02_housing_acc_segreg/table/segregation/globalSegreg.rda')
gSeg %>% 
  filter(measure == 'G3_G3',
         mtype == 'euclidean')

# od jobs analysis ----
# figure 06: bar plot to justify accessibility to jobs ----

od17 %>% 
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
                                           'procurar\nemprego')),
                perc = viagens/sum(viagens))

od17 %>% 
  dplyr::filter(SERVIR_D == 2, MUNI_DOM == 36) %>% 
  dplyr::mutate(motivo = factor(MOTIVO_D,
                                labels = c(rep('trabalho',  3), 
                                           'escola', 'compras', 'saúde',
                                           'lazer', 'residência', 'procurar\nemprego',
                                           'assuntos\npessoais', 'refeição'))) %>% 
  dplyr::filter(motivo=='trabalho') %>% 
  dplyr::group_by(MODOPRIN) %>% 
  dplyr::summarise(viagens = sum(FE_VIA, na.rm=T)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(modo = factor(MODOPRIN,
                              labels = c(rep('tp', 5),
                                         rep('outros', 2),
                                         rep('indmot',6),
                                         rep('ativo', 2),
                                         'outros'))) %>%  
  dplyr::group_by(modo) %>% 
  dplyr::summarise(viagens = sum(viagens, na.rm=T)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(modo = factor(modo,
                              levels = c('tp', 'indmot', 'ativo', 'outros')),
                perc = viagens/sum(viagens))


od17 %>% 
  dplyr::filter(SERVIR_D == 2, MUNI_DOM == 36) %>% 
  dplyr::mutate(motivo = factor(MOTIVO_D,
                                labels = c(rep('trabalho',  3), 
                                           'escola', 'compras', 'saúde',
                                           'lazer', 'residência', 'procurar\nemprego',
                                           'assuntos\npessoais', 'refeição'))) %>% 
  dplyr::filter(motivo=='trabalho',
                RENDA_FA <= 3*958) %>% 
  dplyr::group_by(MODOPRIN) %>% 
  dplyr::summarise(viagens = sum(FE_VIA, na.rm=T)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(modo = factor(MODOPRIN,
                              labels = c(rep('tp', 5),
                                         rep('outros', 1),
                                         rep('indmot',6),
                                         rep('ativo', 2),
                                         'outros'))) %>%  
  dplyr::group_by(modo) %>% 
  dplyr::summarise(viagens = sum(viagens, na.rm=T)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(modo = factor(modo,
                              levels = c('tp', 'indmot', 'ativo', 'outros')),
                perc = viagens/sum(viagens))

# fig 02 - jobs + population----

# * jobs -----
# total jobs
tot <- sum(opportunities$job, na.rm=T)

# breaks of 5 quantiles
brks <- opportunities %>% 
  dplyr::filter(job!=0) %>% 
  .$job %>% 
  quantile(., probs = seq(0,1,1/quant))

brks[1] <- 0

# classifying breaks
plot_jobs <- opportunities %>% 
  dplyr::mutate(range = cut(job, 
                            breaks = brks,
                            include.lowest = T,
                            right = F)) %>% 
  dplyr::group_by(range) %>% 
  dplyr::summarise() %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(var = factor('jobs',
                             levels =  c('jobs', paste0('families of ', c('low', 'medium', 'high')))))

# labels for plot
labs <- as.numeric(stringr::str_replace_all(levels(plot_jobs$range), '\\[|\\)|\\]|.*,',''))
labs <- c(0, formatC(as.numeric(labs[1:4]),format='d',big.mark = ',', decimal.mark = '.'), expression(''%->%''))

# * population ----
pop_micro[is.na(pop_micro)] <- 0

# classifying percentage
pop <- pop_micro %>% 
  dplyr::rename(id = id_grid) %>% 
  dplyr::mutate(tot = g1+g2+g3,
                G1 = g1/tot,
                G2 = g2/tot,
                G3 = g3/tot) %>% 
  dplyr::select(id, G1, G2, G3)

plot_pop <- pop %>%
  tidyr::gather(group, perc, G1:G3) %>% 
  dplyr::mutate(range = as.numeric(cut(perc,
                                       breaks = seq(0,1,0.2),
                                       right = F,
                                       include.lowest = T)) - 0.5) %>% 
  dplyr::left_join(grid) %>% 
  sf::st_as_sf() %>% 
  dplyr::group_by(group, range) %>% 
  dplyr::summarise() %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(group = dplyr::case_when(group == 'G1' ~ 'low',
                                         group == 'G2' ~ 'middle',
                                         TRUE ~ 'high'),
                var = factor(paste0(group,'-class families'),
                             levels = c('jobs', 
                                        paste0(c('low', 'middle', 'high'), 
                                               '-class families'))))

# * plotting figure ----
p <- ggplot() +
  annotation_custom(ggbasemap, xmin=-Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
  # population figure
  geom_sf(data = plot_pop, aes(fill = range), color = NA) +
  scale_fill_stepsn('% families inside cell',
                    colors = colOrange(quant),
                    values = scales::rescale((1:quant)-.5, 
                                             from = c(0,quant)),
                    breaks = 0:quant,
                    labels = seq(0,100,100/quant),
                    limits = c(0,quant),
                    expand = F,
                    right = T,
                    na.value=colOrange(quant),
                    guide = guide_colorbar(order = 3,
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
  new_scale_fill() +
  geom_sf(data = dplyr::mutate(plot_jobs, range = as.numeric(range)-.5),
          aes(fill = range), color = NA) +
  scale_fill_stepsn('jobs (10³)',
                    colors = colOrange(quant),
                    values = scales::rescale((1:quant)-.5,
                                             from = c(0,quant)),
                    breaks = 0:quant,
                    labels = labs,
                    limits = c(0,quant),
                    expand = F,
                    right = T,
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
  geom_sf(data = massa, fill = colWater, color = colWater, size = line.size/500)  +
  geom_sf(data=rios, color=colWater, size=line.size/250) + 
  geom_sf(data=verde, fill=colGreen, color="transparent") +
  geom_sf(data=muni, fill = 'grey40', color = 'transparent', alpha = 0.25) +
  geom_sf(data=trilhos, aes(color = "rails"),
          size = line.size/200, show.legend = "line") +
  labs(caption = bg$caption) +
  scale_color_manual(breaks = "rails",
                     values = c("rails" = "grey40"),
                     guide = guide_legend(title= "", 
                                          order = 1,
                                          title.position = "top")) +
  ggspatial::annotation_scale(location = 'br', 
                              bar_cols = c('grey15', 'grey99'), 
                              line_width = 0.3,
                              line_col = 'grey15',
                              text_col = 'grey15') +
  facet_grid(.~var) +
  theme_bw() +
  theme_map +
  coord_lim

size <- dplyr::filter(map_dim, map=='4')
ggsave(paste0('latex/scs_zeis/figure/design/map_jobs_pop.png'), 
       p, dpi = 500, width = size$width, height = size$height, units = "mm", 
       type = 'cairo-png')

# fig 03 - barplot segregation
segregation <- lSeg %>% 
  dplyr::filter(measure %in% 'G3_G3') %>% 
  dplyr::rename(seg = value)

max <- segregation %>% 
  dplyr::filter(!is.infinite(seg), seg == max(.$seg)) 

brk <- segregation %>% 
  dplyr::filter(bandwd == max$bandwd, mtype == max$mtype) %>% 
  .$seg %>% 
  quantile(.,seq(0,1,1/quant), names=F)

seg_plot <- segregation %>% 
  dplyr::mutate(range = as.numeric(as.character(cut(seg, 
                                                    breaks = brk,
                                                    labels = (1:quant)-0.5,
                                                    include.lowest = T,
                                                    right = T)))) %>% 
  dplyr::right_join(sf::st_drop_geometry(grid))  %>% 
  tidyr::complete(id, tidyr::nesting(mtype, bandwd), fill=list(range = 0.5)) %>% 
  dplyr::filter(!is.na(mtype), !is.na(bandwd)) %>% 
  dplyr::mutate(bandwd = factor(bandwd,
                                levels = c(500, 1500, 5000, 10000),
                                labels = c('500 m', '1,500 m', '5,000 m', '10,000 m')),
                bandwd = forcats::fct_rev(bandwd)) %>% 
  dplyr::group_by(range, bandwd) %>% 
  dplyr::summarise(ct = dplyr::n()) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(bandwd) %>% 
  dplyr::mutate(perc = ct/sum(ct, na.rm=T)) %>% 
  ungroup() %>% 
  mutate(range = as.numeric(range),
         color = if_else(range<2, 'grey90', 'grey10'))

p <- seg_plot %>% 
  ggplot(aes(y = bandwd, 
             x = perc, 
             fill = range)) + 
  geom_bar(position = 'stack', 
           stat = 'identity')+
  scale_fill_stepsn('local isolation (quantile)',
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
  geom_text(aes(label = scales::percent(perc, accuracy = 1), color = color),
            position = position_stack(vjust = 0.5),
            stat = 'identity',
            show.legend = F) +
  scale_color_manual(values=c('grey90', 'grey10'))+
  scale_x_continuous(labels = scales::percent, breaks = seq(0,1,.2))+
  coord_cartesian(expand = F) +
  labs(x= 'percentage', y = 'bandwidth') +
  theme_chart +
  theme(plot.margin = margin(2,7.5,2,2,'mm'))

size <- dplyr::filter(map_dim, map=='chart')
ggsave('latex/scs_zeis/figure/design/barplot_segreg2.png', 
       p, dpi = 500, width = size$width, height = size$height, 
       units = "mm", type = 'cairo-png')
