#' Plots figures 13 to 17
#' 
#' figure 13: heat map  of mcmv
#' figure 14: bivariate analysis
#' figure 15: LISA maps
#' figure 16: scatterplot - mcmv + accessibility and segregation
#' figure 17: boxplot - distribution of accessibility by housing group by segregation percentile 

# Libraries ----
library(magrittr)
library(tidyverse)
library(ggplot2)
library(ggnewscale)
library(cowplot)
base <- 210 # width of images
quant <- 5
source('rscript/00_Maps.R')

# Reading files ----

# accessibility
acc_org <- readr::read_csv('results/02_housing_acc_segreg/table/accessibility_distances.csv') 

acc <- acc_org %>% 
  dplyr::mutate(threshold = paste0(threshold, 'min'),
                acc = perc) %>% 
  dplyr::filter(threshold=='60min', year == '2017') %>% 
  dplyr::rename(id = origin) %>% 
  dplyr::select(id, acc)

acc_aux <- acc_org %>% 
  dplyr::filter(threshold == 60) %>% 
  dplyr::rename(id = origin) %>% 
  dplyr::select(id, year, perc) %>% 
  tidyr::pivot_wider(names_from = year, names_prefix = 'y_', values_from = 'perc')

# isolation
load('results/02_housing_acc_segreg/table/segregation/localSegreg.rda')
segreg <- lSeg %>% 
  dplyr::filter(measure %in% 'G1_G1', bandwd == 1500) %>% 
  dplyr::rename(seg = value) %>% 
  dplyr::select(id, seg)

rm(lSeg)
gc()

# join of accessibility and isolation
df <- dplyr::full_join(acc, segreg) %>% 
  dplyr::mutate(acc = ifelse(is.na(acc), 0, acc),
                seg = ifelse(is.na(seg), 0, seg))

# * creating breaks ----

# accessibility is divided in thirds of the max value
max(df$acc, na.rm=T)
acc_div <- seq(0,.81,length.out = 4)

# segregation is divided into three quantiles
seg_div <- quantile(x = df$seg, probs = seq(0,1,length.out = 4), names = F, na.rm = T)

# * creating the classes ----

df <- df %>%
  dplyr::mutate(seg = ifelse(is.na(seg), 0, seg),
                acc = ifelse(is.na(acc), 0, acc),
                range_acc = as.numeric(cut(acc,
                                           breaks = acc_div,
                                           labels = 1:3,
                                           include.lowest = T,
                                           right = F)),
                range_seg = as.numeric(cut(seg,
                                           breaks = seg_div,
                                           labels = 1:3,
                                           include.lowest = T,
                                           right = F)),
                acc_seg = as.numeric(paste0(range_acc, range_seg)))

seg_m <- median(df$seg)
acc_m <- median(df$acc)

# mcmv
mcmv <- readr::read_delim('data/housing/mcmv/offline/mcmv_sp_2009_2018_edited.csv',
                          locale=readr::locale(encoding='latin1'),
                          delim = ';') %>% 
  dplyr::filter(municipio == 'São Paulo')

mcmv_sf <- sf::st_read('data/shp/mcmv_sf.shp', quiet=T) %>% 
  left_join(df)

# private 
private_org <- sf::st_read('data/housing/private/private.shp', quiet = T) 

private <- private_org %>% 
  sf::st_transform(prjdegrees) %>% 
  dplyr::rename(uh = TT_UNID,
                id = ID)  %>% 
  dplyr::mutate(id = as.character(id),
                class = dplyr::case_when(classe == 'lower' ~ 'HIS', 
                                         classe == 'medium' ~ 'HMP',
                                         TRUE ~ 'alta renda'),
                class = factor(class,
                               levels = c("HIS", 'HMP', 'alta renda')),
                period = stringr::str_replace_all(period, 'Stage', 'fase'),
                origin = 'privado') %>% 
  dplyr::select(id, period, class, uh, origin) %>% 
  sf::st_intersection(grid) %>% 
  dplyr::select(-id) %>% 
  dplyr::rename(id = id.1) %>% 
  left_join(df)

private_df <- private %>% 
  sf::st_drop_geometry()

housing <- mcmv_sf %>% 
  dplyr::rename(period = fase_mcmv,
                class = faixa) %>% 
  dplyr::filter(period != 'Stage 3') %>% 
  dplyr::mutate(class = dplyr::case_when(class == 1 ~ 'HIS',
                                         TRUE ~ 'HMP'),
                origin = 'mcmv',
                period = stringr::str_replace_all(period, 'Stage', 'fase')) %>% 
  sf::st_drop_geometry() %>% 
  rbind(private_df) %>% 
  dplyr::left_join(acc_aux) %>% 
  dplyr::mutate(acc = dplyr::case_when(period == 'fase 1' ~ y_2010,
                                       TRUE ~ y_2014)) %>% 
  dplyr::select(-y_2003, -y_2007, -y_2010, -y_2014, -y_2017)

# numeric analysis ----

mcmv_tot <- mcmv %>% 
  dplyr::summarise(uh_contr = sum(uh_contratadas, na.rm=T),
                   uh_contr_na = sum(ifelse(is.na(lat), uh_contratadas, 0), na.rm = T),
                   perc_uh_contr_na = sum(ifelse(is.na(lat), uh_contratadas, 0), na.rm = T)/uh_contr*100,
                   uh_entr = sum(uh_entregues, na.rm=T),
                   perc_uh_entr = uh_entr/uh_contr*100,
                   uh_entr_na = sum(ifelse(is.na(lat), uh_entregues, 0), na.rm = T),
                   perc_uh_entr_na = sum(ifelse(is.na(lat), uh_entregues, 0), na.rm = T)/uh_entr*100,
                   empreendimentos = dplyr::n(),
                   emp_na = sum(is.na(lat), na.rm = T),
                   perc_emp_na = sum(is.na(lat), na.rm = T)/empreendimentos*100)

mcmv_tot <- mcmv_tot$uh_contr

mcmv_fase <- mcmv %>% 
  dplyr::group_by(fase_mcmv) %>% 
  dplyr::summarise(uh_contr = sum(uh_contratadas, na.rm=T)/mcmv_tot*100,
                   uh_entr = sum(uh_entregues, na.rm=T)/mcmv_tot*100)

mcmv_sum <- mcmv %>% 
  dplyr::group_by(fase_mcmv, faixa) %>% 
  dplyr::summarise(emp = dplyr::n(),
                   uh_contr = sum(uh_contratadas, na.rm=T)/mcmv_tot*100,
                   uh_entr = sum(uh_entregues, na.rm=T)/mcmv_tot*100)

# MCMV ----

# figure 13: heat map  of mcmv----
mcmv_coord <- mcmv_sf %>% 
  rowwise() %>% 
  mutate(lng = unlist(geometry)[1],
         lat = unlist(geometry)[2]) %>% 
  ungroup() %>% 
  sf::st_as_sf() %>% 
  sf::st_drop_geometry()

it <- mcmv_coord %>% 
  filter(faixa != "1,5") %>% 
  group_by(fase_mcmv, faixa) %>% 
  summarise() %>% 
  ungroup() %>% 
  rename(fs = fase_mcmv,
         fx = faixa)

z <- pmap_df(it, function(fs, fx){
  message(fx, fs)
  df <- mcmv_coord %>% 
    filter(faixa == fx, fase_mcmv == fs, !is.na(lng))
  
  message(nrow(df))
  rep <- data.table(df)[,list(lat=rep(lat,uh),lng=rep(lng,uh))]
  message(nrow(rep))
  
  bwd <- 5*10^-2
  n <- 1000
  
  z <- MASS::kde2d(rep$lng, 
                   rep$lat,
                   n = n,
                   h = rep(bwd, n*n),
                   lims = c(-46.826081, -46.365377, 
                            -23.716492, -23.384091))  
  # c(-46.826081, -46.365377, -23.716492, -23.384091)
  
  data.frame(lng = rep(z$x, n),
             lat = rep(z$y, each = n),
             z = melt(z$z),
             fase_mcmv = fs,
             faixa = fx)
})

mcmv_coord <- mcmv_coord %>% 
  mutate(faixa = paste("faixa", faixa),
         fase_mcmv = str_replace(fase_mcmv, "Stage", "fase"))

z <- z %>% 
  mutate(faixa = paste("faixa", faixa),
         fase_mcmv = str_replace(fase_mcmv, "Stage", "fase"))

p <- ggplot() +
  annotation_custom(ggbasemap_short, xmin=-Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
  geom_sf(data = massa, fill = colWater, color = colWater, size = line.size/500)  +
  geom_sf(data=rios, color=colWater, size=line.size/250) +
  geom_sf(data=verde, fill=colGreen, color='transparent') +
  geom_sf(data=muni, fill = 'grey40', color = 'transparent', alpha = 0.25) +
  geom_sf(data=trilhos, aes(fill = 'trilhos'), color = 'grey40',
          size = line.size/200, show.legend = 'line') +
  scale_fill_manual(breaks = c('trilhos'),
                    values = c('trilhos' = 'grey40'),
                    labels = c('trilhos'),
                    guide = guide_legend('',
                                         order = 1,
                                         title.position = 'top',
                                         override.aes = list(shape = NA)))+
  new_scale_fill()+
  geom_contour_filled(data = filter(z, z.value > 1),
                      aes(lng, 
                          lat,
                          z = z.value,
                          fill = stat(level)), 
                      alpha = 0.6,
                      bins=5,
                      geom="polygon") +
  scale_fill_manual('# de UH',
                    values = colOrange(quant),
                    guide = guide_colorsteps(order = 2,
                                             show.limits = T,
                                             title.position = "top",
                                             barheight = base/45,
                                             barwidth = base/3,
                                             default.unit = "mm",
                                             ticks.colour = NA,
                                             override.aes = list(shape = NA,
                                                                 linetype = "blank")))  +
  geom_point(data = filter(mcmv_coord, faixa != "faixa 1,5"), 
             aes(lng, lat),
             color = "grey30",
             alpha = 0.6,
             size = .5) +
  labs(caption = paste0('fase 1: 2009-2010; fase 2: 2011-2014; fase 3: 2015-2018\n',
                        bg$caption))  +
  ggspatial::annotation_scale(location = 'br', 
                              bar_cols = c('grey15', 'grey99'), 
                              line_width = 0.3,
                              line_col = 'grey15',
                              text_col = 'grey15') +
  facet_grid(faixa ~ fase_mcmv, switch = 'y') +
  ggplot2::theme_bw()+
  theme_map +
  coord_lim_short


# p <- isobands %>% 
#   dplyr::mutate(period = factor(period,
#                                 levels = c('Stage 1', 'Stage 2', 'Stage 3'),
#                                 labels = c('fase 1', 'fase 2', 'fase 3')),
#                 class = paste('faixa',class)) %>% 
#   ggplot() +
#   annotation_custom(ggbasemap_short, xmin=-Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
#   geom_sf(data = massa, fill = colWater, color = colWater, size = line.size/500)  +
#   geom_sf(data=rios, color=colWater, size=line.size/250) + 
#   geom_sf(data=verde, fill=colGreen, color='transparent') +
#   geom_sf(data=muni, fill = 'grey40', color = 'transparent', alpha = 0.25)+
#   geom_sf(aes(fill = level), alpha = 0.6, color = NA)+
#   scale_fill_stepsn('# de UH',
#                     colors = colOrange(quant),
#                     values = scales::rescale((1:quant)-.5, 
#                                              from = c(0,quant)),
#                     breaks = (0:quant)*10,
#                     labels = (0:quant)*10,
#                     limits = c(0, quant*10),
#                     expand = F,
#                     right = T,
#                     guide = guide_colorsteps(order = 2,
#                                              even.steps = F,
#                                              title.position = "top",
#                                              barheight = base/45,
#                                              barwidth = base/3,
#                                              default.unit = "mm",
#                                              ticks.colour = NA,
#                                              override.aes = list(shape = NA,
#                                                                  linetype = "blank")))+
#   new_scale_fill()+
#   geom_sf(data=trilhos, aes(fill = 'trilhos'), color = 'grey40',
#           size = line.size/200, show.legend = 'line') +
#   labs(caption = paste0('fase 1: 2009-2010; fase 2: 2011-2014; fase 3: 2015-2018\n',
#                         bg$caption))  +
#   scale_fill_manual(breaks = c('trilhos'),
#                     values = c('trilhos' = 'grey40'),
#                     labels = c('trilhos'),
#                     guide = guide_legend('',
#                                          order = 1,
#                                          title.position = 'top',
#                                          override.aes = list(shape = NA)))+
#   ggspatial::annotation_scale(location = 'br', 
#                               bar_cols = c('grey15', 'grey99'), 
#                               line_width = 0.3,
#                               line_col = 'grey15',
#                               text_col = 'grey15') +
#   facet_grid(class ~ period, switch = 'y')+
#   ggplot2::theme_bw()+
#   theme_map +
#   coord_lim_short

size <- dplyr::filter(map_dim, map=='3_short')
ggsave(paste0('latex/dissertacao_bms_vfull/figuras/03_resultados/01_mcmv/fig13_map_mcmv_heatmap.png'), 
       p, dpi = 500, type = 'cairo-png', width = size$width, height = size$height, units = 'mm')


# figure 14: bivariate analysis ----

# number and percent of cells in each class
summary_cells <- df %>% 
  dplyr::group_by(acc_seg) %>% 
  dplyr::summarise(n = dplyr::n()) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(perc = n/sum(n, na.rm=T)*100)

# correlation 
cor(df$acc, df$seg)

# * bivariate maps ----
# colors of accessibility and segregation
col_acc <- unname(colBivariate[1:3])
col_segreg <- unname(colBivariate[seq(1,9,3)])

# dissolve 
plot <- df %>% 
  dplyr::filter(!is.na(acc), !is.na(seg)) %>% 
  dplyr::select(-acc, -seg) %>% 
  tidyr::pivot_longer(c(acc_seg,range_seg, range_acc), names_to = 'plot', values_to = 'value') %>% 
  dplyr::mutate(plot = factor(plot,
                              levels = c('range_acc', 'range_seg', 'acc_seg'),
                              labels = c('acessibilidade', 'segregação', 'bivariado'))) %>% 
  dplyr::right_join(grid) %>% 
  sf::st_as_sf() %>% 
  dplyr::group_by(plot, value) %>% 
  dplyr::summarise() %>% 
  dplyr::ungroup() %>% 
  dplyr::filter(!is.na(plot)) %>% 
  dplyr::mutate(value = as.numeric(value))

# accessibility filter
acc_plot <- dplyr::filter(plot, plot == 'acessibilidade') %>% 
  dplyr::mutate(value = value-.5)

# 
# acc_plot <- dplyr::filter(plot, plot == 'acessibilidade') %>% 
#   dplyr::mutate(value = dplyr::case_when(value == 1 ~ acc_div[2]/2,
#                                          value == 2 ~ (acc_div[4]/2),
#                                          TRUE ~ acc_div[4]-acc_div[2]/2))

# segregation filter
seg_plot <- dplyr::filter(plot, plot == 'segregação') %>% 
  dplyr::mutate(value = value-.5)

# bivariate filter
bi_plot <- dplyr::filter(plot, plot == 'bivariado')

# maps

p <- ggplot() +
  annotation_custom(ggbasemap, xmin=-Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
  # accessibility map
  geom_sf(data = acc_plot, aes(fill = value), color = NA) +
  scale_fill_stepsn(name = '% do total de empregos',
                    colors = col_acc,
                    values = scales::rescale((1:2)-.5, 
                                             from = c(0,2)),
                    breaks = 0:2,
                    labels = round(c(0, acc_m, max(df$acc))*100,0),
                    limits = c(0, 2),
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
                                                                 linetype = "blank"))) +
  # segregation map
  new_scale_fill() +
  geom_sf(data = seg_plot, aes(fill = value), color = NA) +
  scale_fill_stepsn(name = 'isolamento local de G1',
                    colors = col_segreg,
                    breaks = 0:2,
                    values = scales::rescale((1:2)-.5, 
                                             from = c(0,2)),
                    labels = scales::scientific(round(c(0, seg_m, max(df$seg)),6), digits = 1),
                    limits = c(0,2),
                    expand = F,
                    right = T,
                    guide = guide_colorsteps(order = 3,
                                             even.steps = F,
                                             title.position = "top",
                                             barheight = base/45,
                                             barwidth = base/3,
                                             default.unit = "mm",
                                             ticks.colour = NA,
                                             override.aes = list(shape = NA,
                                                                 linetype = "blank"))) +
  # bivariate plot
  new_scale_fill() +
  geom_sf(data = bi_plot, aes(fill = as.character(value)), color = NA, show.legend = F) +
  scale_fill_manual(values = colBivariate,
                    guide = guide_legend(order = 3,
                                         nbin = 200,
                                         title.position = "top",
                                         label.position = "bottom",
                                         barheight = base/45,
                                         barwidth = base/3,
                                         default.unit = "mm",
                                         ticks.colour = NA,
                                         override.aes = list(shape = NA,
                                                             linetype = "blank")),
                    aesthetics = "fill") +
  geom_sf(data = massa, fill = colWater, color = colWater, size = line.size/500)  +
  geom_sf(data=rios, color=colWater, size=line.size/250) + 
  geom_sf(data=verde, fill=colGreen, color="transparent") +
  geom_sf(data=trilhos, aes(color = "trilhos"),
          size = line.size/200, show.legend = F) +
  geom_sf(data=muni, fill = 'grey40', color = 'transparent', alpha = 0.25) +
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
  labs(caption = paste0(bg$caption))+
  facet_grid(.~plot)+
  theme_bw() +
  theme_map_3 +
  coord_lim 


p <- ggplot() +
  annotation_custom(ggbasemap, xmin=-Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
  # accessibility map
  geom_sf(data = acc_plot, aes(fill = value), color = NA) +
  scale_fill_stepsn(name = '% do total de empregos',
                    colors = col_acc,
                    values = scales::rescale((1:3)-.5, 
                                             from = c(0,3)),
                    breaks = acc_div,
                    labels = acc_div*100,
                    limits = c(0, 0.81),
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
                                                                 linetype = "blank"))) +
  # segregation map
  new_scale_fill() +
  geom_sf(data = seg_plot, aes(fill = value), color = NA) +
  scale_fill_stepsn(name = 'isolamento local de G1',
                    colors = col_segreg,
                    breaks = 0:3,
                    values = scales::rescale((1:3)-.5, 
                                             from = c(0,3)),
                    labels = scales::scientific(round(seg_div,6), digits = 1),
                    limits = c(0,3),
                    expand = F,
                    right = T,
                    guide = guide_colorsteps(order = 3,
                                             even.steps = F,
                                             title.position = "top",
                                             barheight = base/45,
                                             barwidth = base/3,
                                             default.unit = "mm",
                                             ticks.colour = NA,
                                             override.aes = list(shape = NA,
                                                                 linetype = "blank"))) +
  # bivariate plot
  new_scale_fill() +
  geom_sf(data = bi_plot, aes(fill = as.character(value)), color = NA, show.legend = F) +
  scale_fill_manual(values = colBivariate,
                    guide = guide_legend(order = 3,
                                         nbin = 200,
                                         title.position = "top",
                                         label.position = "bottom",
                                         barheight = base/45,
                                         barwidth = base/3,
                                         default.unit = "mm",
                                         ticks.colour = NA,
                                         override.aes = list(shape = NA,
                                                             linetype = "blank")),
                    aesthetics = "fill") +
  geom_sf(data = massa, fill = colWater, color = colWater, size = line.size/500)  +
  geom_sf(data=rios, color=colWater, size=line.size/250) + 
  geom_sf(data=verde, fill=colGreen, color="transparent") +
  geom_sf(data=trilhos, aes(color = "trilhos"),
          size = line.size/200, show.legend = F) +
  geom_sf(data=muni, fill = 'grey40', color = 'transparent', alpha = 0.25) +
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
  labs(caption = paste0(bg$caption))+
  facet_grid(.~plot)+
  theme_bw() +
  theme_map_3 +
  coord_lim 

# * legends ----
legend_bi <- df %>% 
  ggplot() +
  geom_rect(aes(xmin = acc_div[1], ymin = seg_div[1], xmax = acc_div[2], ymax = seg_div[2]), fill = colBivariate['11'])+
  geom_rect(aes(xmin = acc_div[2], ymin = seg_div[1], xmax = acc_div[3], ymax = seg_div[2]), fill = colBivariate['21'])+
  geom_rect(aes(xmin = acc_div[3], ymin = seg_div[1], xmax = acc_div[4], ymax = seg_div[2]), fill = colBivariate['31'])+
  geom_rect(aes(xmin = acc_div[1], ymin = seg_div[2], xmax = acc_div[2], ymax = seg_div[3]), fill = colBivariate['12'])+
  geom_rect(aes(xmin = acc_div[2], ymin = seg_div[2], xmax = acc_div[3], ymax = seg_div[3]), fill = colBivariate['22'])+
  geom_rect(aes(xmin = acc_div[3], ymin = seg_div[2], xmax = acc_div[4], ymax = seg_div[3]), fill = colBivariate['32'])+
  geom_rect(aes(xmin = acc_div[1], ymin = seg_div[3], xmax = acc_div[2], ymax = seg_div[4]), fill = colBivariate['13'])+
  geom_rect(aes(xmin = acc_div[2], ymin = seg_div[3], xmax = acc_div[3], ymax = seg_div[4]), fill = colBivariate['23'])+
  geom_rect(aes(xmin = acc_div[3], ymin = seg_div[3], xmax = acc_div[4], ymax = seg_div[4]), fill = colBivariate['33'])+
  geom_point(aes(x=acc, y = seg), color = 'white', alpha = 0.02, size = 0.01) +
  scale_y_continuous(breaks = seg_div, 
                     trans = scales::pseudo_log_trans(10^-06, base=10),
                     labels = scales::scientific(round(seg_div,6), digits = 1)) +
  scale_x_continuous(breaks = acc_div, labels = acc_div*100) +
  coord_flip(expand = F) +
  labs(x = "> acessibilidade",
       y = "> segregação") +
  theme_chart +
  theme(axis.line = element_blank(),
        plot.background = element_blank(),
        panel.border = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_text(size = font.size*0.08))

# gathering maps and legend
size <- dplyr::filter(map_dim, map=='4')

p2 <- ggdraw(p) +
  draw_plot(legend_bi, 
            x = 1-27/size$width, y = 38/size$height, 
            width = 27/size$width, height = 27/size$height, hjust = 1) 
ggsave('latex/dissertacao_bms_vfull/figuras/03_resultados/01_mcmv/map_bivariate2.png', 
       p, dpi = 500, width = size$width, height = size$height, units = "mm", type='cairo-png')

# figure 15: LISA maps ----

# * moran ----
# moran_aux <- df %>% 
#   dplyr::select(id, acc, seg) %>% 
#   dplyr::right_join(grid) %>% 
#   sf::st_as_sf() %>% 
#   dplyr::mutate(seg = ifelse(is.na(seg), 0, seg),
#                 acc = ifelse(is.na(acc), 0, acc))

#moran <- accSeg:::globalMoran(moran_aux)
#moran_sim <- ecdf(accSeg:::globalMoranSim(moran_aux))
#moran_sim(moran)

#lisa_shp <- accSeg:::LISAmaps(moran_aux)

#rgdal::writeOGR(sf::as_Spatial(sf::st_as_sf(lisa_shp)), 
# 'results/02_housing_acc_segreg/lisa.shp', layer = 'lisa',
# driver = 'ESRI Shapefile')

# * map ----
lisa_shp <- sf::st_read('results/02_housing_acc_segreg/lisa.shp', quiet=T) %>% 
  sf::st_transform(prjdegrees) %>% 
  dplyr::rename(agrupamento = patterns,
                `significância` = pvalue) %>% 
  dplyr::select(id, agrupamento, `significância`) %>% 
  tidyr::pivot_longer(c(agrupamento, `significância`), names_to = 'var', values_to = 'value') %>% 
  sf::st_as_sf() %>% 
  dplyr::group_by(var, value) %>% 
  dplyr::summarise() %>% 
  dplyr::ungroup() %>% 
  dplyr::filter(!is.na(value)) 

cluster_p <- dplyr::filter(lisa_shp, var == 'agrupamento')
pvalue_p <- dplyr::filter(lisa_shp, var == 'significância')

p <- lisa_shp %>% 
  ggplot() +
  annotation_custom(ggbasemap, xmin=-Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
  geom_sf(data = cluster_p, aes(fill = value), color = NA) +
  scale_fill_manual(name = 'agrupamento',
                    breaks = c('High - High',
                               'High - Low',
                               'Not significant',
                               'Low - High',
                               'Low - Low'),
                    values = c('High - High' = colRedBlue(5)[1],
                               'High - Low' = colRedBlue(5)[2],
                               'Not significant' = colRedBlue(5)[3],
                               'Low - High' = colRedBlue(5)[4],
                               'Low - Low' = colRedBlue(5)[5]),
                    labels = c('alto - alto',
                               'alto - baixo',
                               'não significante',
                               'baixo - alto',
                               'baixo - baixo'),
                    guide = guide_legend(order = 2,
                                         nrow = 1,
                                         title.position = "top",
                                         label.position = "bottom",
                                         barheight = base/45,
                                         barwidth = base/3,
                                         default.unit = "mm",
                                         override.aes = list(shape = NA,
                                                             linetype = "blank",
                                                             fill = colRedBlue(5)))) +
  new_scale_fill() +
  geom_sf(data = pvalue_p, aes(fill = value), color = NA) +
  scale_fill_manual(name = 'significância',
                    breaks = c('p = 0.001',
                               'p = 0.01',
                               'p = 0.05',
                               'Not significant'),
                    values = c('p = 0.001' = colOrange(5)[3],
                               'p = 0.01' = colOrange(5)[2],
                               'p = 0.05' = colOrange(5)[1],
                               'Not significant' = colRedBlue(5)[3]),
                    labels = c('p = 0.001',
                               'p = 0.01',
                               'p = 0.05',
                               'não significante'),
                    guide = guide_legend(order = 3,
                                         nrow = 1,
                                         title.position = "top",
                                         label.position = "bottom",
                                         barheight = base/45,
                                         barwidth = base/3,
                                         default.unit = "mm",
                                         override.aes = list(shape = NA,
                                                             linetype = "blank"))) +
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
  facet_grid(~var) +
  theme_bw() +
  theme_map_2 +
  theme(legend.spacing.x = unit(.1,'cm')) +
  coord_lim 

size <- dplyr::filter(map_dim, map=='4')
ggsave(paste0('latex/dissertacao_bms_vfull/figuras/resultados/mcmv/map_lisa_acc_segreg.png'),
       p, dpi = 500, width = size$width, height = size$height, units = "mm", type = 'cairo-png')

# figure 16: scatterplot - mcmv + accessibility and segregation ----

# * numeric analysis ----

hs_df <- mcmv_sf %>%
  dplyr::filter(faixa != '1,5') 

summary_mcmv <- hs_df %>% 
  sf::st_drop_geometry() %>% 
  dplyr::group_by(faixa, acc_seg) %>% 
  dplyr::summarise(n = dplyr::n()) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(faixa) %>% 
  dplyr::mutate(perc = n/sum(n, na.rm=T)*100) %>% 
  dplyr::filter(acc_seg == 13)

summary_mcmv

summary_mcmv <- hs_df %>% 
  sf::st_drop_geometry() %>% 
  dplyr::group_by(faixa, fase_mcmv, acc_seg) %>% 
  dplyr::summarise(n = dplyr::n()) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(faixa, fase_mcmv) %>% 
  dplyr::mutate(perc = n/sum(n, na.rm=T)*100) %>% 
  dplyr::filter(faixa == 1)

summary_mcmv

summary_mcmv <- hs_df %>% 
  sf::st_drop_geometry() %>% 
  dplyr::mutate(racc = ifelse(acc>=acc_m, "alto", "baixo"),
                rseg = ifelse(seg>=seg_m, "alto", "baixo"),
                acc_seg = paste(racc,rseg,sep="-")) %>% 
  dplyr::group_by(faixa, fase_mcmv, acc_seg) %>% 
  dplyr::summarise(n = dplyr::n()) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(faixa, fase_mcmv) %>% 
  dplyr::mutate(perc = n/sum(n, na.rm=T)*100) 

summary_mcmv

summary_mcmv <- hs_df %>% 
  sf::st_drop_geometry() %>% 
  dplyr::mutate(racc = ifelse(acc>=acc_m, "alto", "baixo"),
                rseg = ifelse(seg>=seg_m, "alto", "baixo"),
                acc_seg = paste(racc,rseg,sep="-")) %>% 
  dplyr::group_by(faixa, fase_mcmv, acc_seg) %>% 
  dplyr::summarise(n = dplyr::n()) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(faixa, fase_mcmv) %>% 
  dplyr::mutate(perc = n/sum(n, na.rm=T)*100) 

summary_mcmv

# * scatterplot ----

p <- hs_df %>% 
  dplyr::select(seg, acc, uh, faixa, fase_mcmv) %>% 
  dplyr::mutate(faixa = paste('faixa', faixa),
                fase_mcmv = stringr::str_replace_all(fase_mcmv, 'Stage', 'fase')) %>% 
  #dplyr::filter(fase_mcmv == "fase 1", faixa == "faixa 1") %>% 
  ggplot() +
  geom_point(data=df, aes(acc*100, seg), alpha = 0.08, color = 'grey90') +
  geom_point(aes(acc*100, seg, color = faixa, size = uh), alpha = 0.4) +
  geom_vline(aes(xintercept = acc_m*100), color = "grey30") +
  geom_hline(aes(yintercept = seg_m), color = "grey30")+
  scale_color_manual('faixa',
                     breaks = c('faixa 1', 'faixa 2', 'faixa 3'),
                     labels = 1:3,
                     values = colOrange(5)[c(1,3,4)]) +
  scale_size(range = c(0,10)) +
  scale_y_continuous(breaks = c(seg_div, seg_m), 
                     trans = scales::pseudo_log_trans(10^-06, base=10),
                     labels = scales::scientific(round(c(seg_div, seg_m),6), digits = 1)) +
  scale_x_continuous(breaks = c(acc_div, acc_m)*100, 
                     labels = scales::percent_format(scale=1)) +
  guides(color = guide_legend('faixa',
                              title.position = 'top',
                              order = 1,
                              override.aes = list(
                                size = 3,
                                alpha = .8
                              )),
         size = guide_legend('UH',
                             title.position = 'top',
                             order = 2)) +
  coord_flip(expand = F, xlim = c(0,81)) +
  labs(y = 'isolamento de G1', 
       x = 'acessibilidade (% do total de empregos)',
       caption = 'fase 1: 2009-2010; fase 2: 2011-2014; fase 3: 2015-2018') +
  facet_grid(faixa~fase_mcmv)+
  theme_chart +
  theme(panel.grid.minor.y = element_blank(),
        panel.spacing.x = unit(10,'mm'),
        panel.spacing.y = unit(4,'mm'),
        plot.caption.position = 'plot',
        axis.text.x = element_text(angle = 30, hjust = 1))

size <- dplyr::filter(map_dim, map=='chart')
ggsave(paste0('latex/dissertacao_bms_vfull/figuras/03_resultados/01_mcmv/scatterplot_housing_acc_seg2.png'),
       plot = p, dpi = 500, width = size$width, height = size$width, units = "mm", type = 'cairo-png')

# figure 17: boxplot - distribution of accessibility by housing group by segregation percentile  ----

hs_df %>% 
  dplyr::mutate(faixa = paste('faixa', faixa),
                fase_mcmv = stringr::str_replace_all(fase_mcmv, 'Stage', 'fase')) %>% 
  dplyr::filter(!is.na(range_seg)) %>% 
  sf::st_drop_geometry() %>% 
  dplyr::bind_rows(data.frame(range_seg = rep(c(0, 1.5, 2.5, 3.5), 3),
                              fase_mcmv = rep(c('fase 1', 'fase 2', 'fase 3'), each = 4))) %>%  
  ggplot()+
  geom_boxplot(aes(x = factor(range_seg), y = acc, fill = faixa), 
               alpha = 0.4, color = 'grey40', lwd = line.size*.005) +
  scale_fill_manual('faixa',
                    breaks = c('faixa 1', 'faixa 2', 'faixa 3'),
                    labels = 1:3,
                    values = colOrange(5)[c(1,3,4)]) +
  scale_y_continuous(breaks = c(0,.27,.54, .81), limits = c(0,.81), labels = scales::percent)+
  scale_x_discrete(breaks = c('0','1.5', '2.5','3.5'), 
                   labels = c(0,scales::scientific(round(seg_div[-1],6), digits = 1)))+
  coord_cartesian(expand=F)+
  labs(x = 'isolamento de G1', 
       y = 'acessibilidade',
       caption = 'fase 1: 2009-2010; fase 2: 2011-2014; fase 3: 2015-2018') +
  facet_grid(~fase_mcmv) +
  theme_chart +
  theme(panel.spacing.x = unit(10,'mm'),
        plot.margin = margin(2,7.5,2,2,'mm'))

size <- dplyr::filter(map_dim, map=='chart')
ggsave(paste0('latex/dissertacao_bms_vfull/figuras/resultados/mcmv/boxplot_housing_acc_seg.png'),
       dpi = 500, width = size$width, height = size$height, units = "mm")


# PRIVATE ----

# figure 18: boxplot price per area ----

private_org %>% 
  sf::st_drop_geometry() %>% 
  dplyr::select(ANO_LAN, PC_AU_ATU, AR_UT_UNID) %>%
  dplyr::rename(ano = ANO_LAN,
                preco_au = PC_AU_ATU,
                au = AR_UT_UNID) %>% 
  tidyr::pivot_longer(c(preco_au, au), names_to = 'var', values_to = 'value') %>% 
  ggplot(aes(x=ano, y=value)) +
  geom_violin(color = 'grey70') +
  geom_boxplot(color = 'grey40', fill = 'transparent', alpha = 0.3, lwd = line.size/200)+
  scale_y_continuous(labels = scales::label_number(big.mark = '.', decimal.mark = ',')) +
  facet_wrap(~var, 
             scales = 'free_y',
             strip.position = 'left',
             labeller = as_labeller(c(au = 'área útil (m²)',
                                      preco_au = 'preço por área útil (R$/m²)')))+
  labs(x = 'ano', y=NULL)+
  theme_chart +
  theme(strip.placement = 'outside')

size <- dplyr::filter(map_dim, map=='chart')
ggsave('latex/dissertacao_bms_vfull/figuras/03_resultados/01_mcmv/boxplot_price_area.png',
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
  scale_fill_manual('classe',
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
                               rep('alta renda', 7)),
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

private_coord <- private_org %>% 
  rowwise() %>% 
  mutate(lng = unlist(geometry)[1],
         lat = unlist(geometry)[2]) %>% 
  ungroup() %>% 
  sf::st_as_sf() %>% 
  sf::st_drop_geometry()

it <- private_coord %>% 
  group_by(period, classe) %>% 
  summarise() %>% 
  ungroup() %>% 
  rename(fs = period,
         fx = classe)

z <- pmap_df(it, function(fs, fx){
  message(fx, fs)
  df <- private_coord %>% 
    filter(classe == fx, period == fs, !is.na(lng))
  
  message(nrow(df))
  rep <- data.table(df)[,list(lat=rep(lat,TT_UNID),lng=rep(lng,TT_UNID))]
  message(nrow(rep))
  
  bwd <- 5*10^-2
  n <- 1000
  
  z <- MASS::kde2d(rep$lng, 
                   rep$lat,
                   n = n,
                   h = rep(bwd, n*n),
                   lims = c(-46.826081, -46.365377, 
                            -23.716492, -23.384091))  
  # c(-46.826081, -46.365377, -23.716492, -23.384091)
  
  data.frame(lng = rep(z$x, n),
             lat = rep(z$y, each = n),
             z = melt(z$z),
             period = fs,
             classe = fx)
})

private_coord <- private_coord %>% 
  mutate(period = str_replace(period, "Stage", "fase"),
         classe = factor(classe,
                         levels = c("medium", "higher"),
                         labels = c("HMP", "alta renda")))

z <- z %>% 
  mutate(period = str_replace(period, "Stage", "fase"),
         classe = factor(classe,
                         levels = c("medium", "higher"),
                         labels = c("HMP", "alta renda")))

p <- ggplot() +
  annotation_custom(ggbasemap_short, xmin=-Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
  geom_sf(data = massa, fill = colWater, color = colWater, size = line.size/500)  +
  geom_sf(data=rios, color=colWater, size=line.size/250) +
  geom_sf(data=verde, fill=colGreen, color='transparent') +
  geom_sf(data=muni, fill = 'grey40', color = 'transparent', alpha = 0.25) +
  geom_sf(data=trilhos, aes(fill = 'trilhos'), color = 'grey40',
          size = line.size/200, show.legend = 'line') +
  scale_fill_manual(breaks = c('trilhos'),
                    values = c('trilhos' = 'grey40'),
                    labels = c('trilhos'),
                    guide = guide_legend('',
                                         order = 1,
                                         title.position = 'top',
                                         override.aes = list(shape = NA)))+
  new_scale_fill()+
  geom_contour_filled(data = filter(z, z.value > 1),
                      aes(lng, 
                          lat,
                          z = z.value,
                          fill = stat(level)), 
                      alpha = 0.6,
                      bins=5,
                      geom="polygon") +
  scale_fill_manual('# de UH',
                    values = colOrange(quant),
                    guide = guide_colorsteps(order = 2,
                                             show.limits = T,
                                             title.position = "top",
                                             barheight = base/45,
                                             barwidth = base/3,
                                             default.unit = "mm",
                                             ticks.colour = NA,
                                             override.aes = list(shape = NA,
                                                                 linetype = "blank")))  +
  geom_point(data = private_coord, 
             aes(lng, lat),
             color = "grey30",
             alpha = 0.4,
             size = .5) +
  labs(caption = paste0('fase 1: 2009-2010; fase 2: 2011-2014\n',
                        bg$caption))  +
  ggspatial::annotation_scale(location = 'br', 
                              bar_cols = c('grey15', 'grey99'), 
                              line_width = 0.3,
                              line_col = 'grey15',
                              text_col = 'grey15') +
  facet_grid(classe ~ period, switch = 'y') +
  ggplot2::theme_bw()+
  theme_map +
  coord_lim_short

size <- dplyr::filter(map_dim, map=='2')
ggsave(paste0('latex/dissertacao_bms_vfull/figuras/03_resultados/02_zeis/map_private_heatmap.png'), 
       p, dpi = 500, type = 'cairo-png', width = size$width, height = size$height+5, units = 'mm')  

# new figure - scatterplot embraesp x acc x segreg ----

# * numeric analysis ----

summary <- private_df %>% 
  dplyr::mutate(racc = ifelse(acc>=acc_m, "alto", "baixo"),
                rseg = ifelse(seg>=seg_m, "alto", "baixo"),
                acc_seg = paste(racc,rseg,sep="-")) %>% 
  dplyr::group_by(class, period, acc_seg) %>% 
  dplyr::summarise(n = dplyr::n()) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(class, period) %>% 
  dplyr::mutate(perc = n/sum(n, na.rm=T)*100) 

summary

# * scatter ----

p <- private_df  %>% 
  dplyr::group_by(id, period, class, seg, acc) %>% 
  dplyr::summarise(uh = sum(uh)) %>% 
  dplyr::ungroup() %>% 
  #dplyr::filter(period == "fase 1", class == "HMP") %>% 
  ggplot() +
  geom_point(data=df, aes(acc*100, seg), alpha = 0.08, color = 'grey90') +
  geom_point(aes(acc*100, seg, color = class, size = uh), alpha = 0.4) +
  geom_vline(aes(xintercept = acc_m*100), color = "grey30") +
  geom_hline(aes(yintercept = seg_m), color = "grey30")+
  scale_color_manual('classe',
                     breaks = c('HIS', 'HMP', 'alta renda'),
                     labels = c('HIS', 'HMP', 'alta renda'),
                     values = colOrange(5)[c(1,3,5)]) +
  scale_size(range = c(0,10)) +
  scale_y_continuous(breaks = c(seg_div, seg_m), 
                     trans = scales::pseudo_log_trans(10^-06, base=10),
                     labels = scales::scientific(round(c(seg_div, seg_m),6), digits = 1)) +
  scale_x_continuous(breaks = c(acc_div, acc_m)*100, 
                     labels = scales::percent_format(scale=1)) +
  guides(color = guide_legend('classe',
                              title.position = 'top',
                              order = 1,
                              override.aes = list(
                                size = 3,
                                alpha = .8
                              )),
         size = guide_legend('UH',
                             title.position = 'top',
                             order = 2)) +
  coord_flip(expand = F, xlim = c(0,81)) +
  labs(y = 'isolamento de G1', 
       x = 'acessibilidade (% do total de empregos)',
       caption = 'fase 1: 2009-2010; fase 2: 2011-2014') +
  facet_grid(class~period)+
  theme_chart +
  theme(panel.grid.minor.y = element_blank(),
        panel.spacing.x = unit(10,'mm'),
        panel.spacing.y = unit(4,'mm'),
        plot.caption.position = 'plot',
        axis.text.x = element_text(angle = 30, hjust = 1))

size <- dplyr::filter(map_dim, map=='chart_short')
ggsave(paste0('latex/dissertacao_bms_vfull/figuras/03_resultados/01_mcmv/scatterplot_private_acc_seg2.png'),
       plot = p, dpi = 500, width = 150, height = 210*.75, units = "mm", type = 'cairo-png')

# new - scatter all ----

p <- housing  %>% 
  dplyr::mutate(class = factor(class,
                               levels = c('HIS', 'HMP', 'alta renda'))) %>% 
  dplyr::group_by(id, period, class, seg, acc, origin) %>% 
  dplyr::summarise(uh = sum(uh)) %>% 
  dplyr::ungroup() %>% 
  #dplyr::filter(period == "fase 1", class == "HMP") %>% 
  ggplot() +
  geom_point(data=df, aes(acc*100, seg), alpha = 0.08, color = 'grey90') +
  geom_point(aes(acc*100, seg, color = class, size = uh, shape = origin), alpha = 0.4) +
  geom_vline(aes(xintercept = acc_m*100), color = "grey30") +
  geom_hline(aes(yintercept = seg_m), color = "grey30")+
  scale_color_manual('classe',
                     breaks = c('HIS', 'HMP', 'alta renda'),
                     labels = c('HIS', 'HMP', 'alta renda'),
                     values = colOrange(5)[c(1,3,5)]) +
  scale_size(range = c(0,10)) +
  scale_y_continuous(breaks = c(seg_div, seg_m), 
                     trans = scales::pseudo_log_trans(10^-06, base=10),
                     labels = scales::scientific(round(c(seg_div, seg_m),6), digits = 1)) +
  scale_x_continuous(breaks = c(acc_div, acc_m)*100, 
                     labels = scales::percent_format(scale=1)) +
  guides(color = guide_legend('classe',
                              title.position = 'top',
                              order = 1,
                              override.aes = list(
                                size = 3,
                                alpha = .8
                              )),
         size = guide_legend('UH',
                             title.position = 'top',
                             order = 2)) +
  coord_flip(expand = F, xlim = c(0,81)) +
  labs(y = 'isolamento de G1', 
       x = 'acessibilidade (% do total de empregos)',
       caption = 'fase 1: 2009-2010; fase 2: 2011-2014') +
  facet_grid(class~period)+
  theme_chart +
  theme(panel.grid.minor.y = element_blank(),
        panel.spacing.x = unit(10,'mm'),
        panel.spacing.y = unit(4,'mm'),
        plot.caption.position = 'plot',
        axis.text.x = element_text(angle = 30, hjust = 1))

p

# new - logistic regression ----

housing_z <- housing
housing_z$z_acc <- scale(housing$acc)
housing_z$z_seg <- scale(housing$seg)

fit <- nnet::multinom(class ~ z_acc + z_seg, data = housing_z)

fit

# figure 21: accessibility - relative scale ----

# treating data
df_acc <- acc_org %>% 
  dplyr::mutate(range = as.numeric(as.character(factor(cut(acc,
                                                           breaks = seq(0,1,1/quant),
                                                           label = (1:quant)-.5,
                                                           include.lowest = T,
                                                           right = F))))) %>% 
  dplyr::rename(id = origin) %>% 
  dplyr::right_join(sf::st_drop_geometry(grid)) %>% 
  tidyr::complete(id, fill=list(range = 0.5, acc = 0, acc = 0)) %>% 
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
                               levels = c('HIS', 'HMP', 'alta renda'),
                               labels = c('HIS', 'HMP', 'alta renda'))) %>% 
  ggplot(aes(x = origin, y = acc))+
  #geom_violin(aes(color = class), scale = 'count') +
  geom_boxplot(aes(fill = class), alpha = 0.3, color = 'grey40', lwd = line.size*.005) +
  scale_fill_manual('classe',
                    breaks = c('HIS', 'HMP', 'alta renda'),
                    values = colOrange(5)[c(1,3,5)],
                    aesthetics = c("fill", "color")) +
  scale_y_continuous(labels = scales::percent, limits = c(0,1), expand = c(0,0)) +
  labs(x='', y='% do total de empregos',
       caption = 'fase 1: 2009-2010; fase 2: 2011-2014') + 
  facet_grid(~period) +
  theme_chart 

size <- dplyr::filter(map_dim, map=='chart')
ggsave(paste0('latex/dissertacao_bms_vfull/figuras/03_resultados/01_mcmv/boxplot_housing_acc.png'),
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
                               levels = c('HIS', 'HMP', 'alta renda'))) %>% 
  ggplot() +
  geom_line(aes(x=cumuh, y=cumacc), 
            lwd = line.size*.01)+
  geom_abline(intercept=0, slope=1, color="grey50", 
              lwd = line.size*.008, lty = 'dashed') +
  geom_point(data = median, aes(x = median.x, y = median.y, color = class, shape = origin), size = 4) +
  geom_text(data = gini_base, aes(x = 0.2, y = 0.95, 
                                  label = paste0('Gini: ', format(gini,digits=4,decimal.mark=',')))) +
  scale_color_manual('classe', 
                     breaks = c('HIS', 'HMP', 'alta renda'),
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
ggsave("latex/dissertacao_bms_vfull/figuras/03_resultados/01_mcmv/lorenz_acc_uh.png", 
       p, dpi = 500, width = size$width*.7, height = size$height, units = "mm")
