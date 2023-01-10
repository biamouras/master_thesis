# Plots figures of appendixes 

# Library ----
library(tidyverse)
base <- 210
quant <- 5
source('rscript/00_Maps.R')

# Reading files ----
opportunities_org <- sf::st_read('data/opportunities/jobs_grid_nereus.shp', quiet = T) %>% 
  sf::st_transform(prjdegrees) %>% 
  dplyr::rename(job = emprego,
                year = ano)

# accessibility
accessibility <- readr::read_csv('results/02_housing_acc_segreg/table/accessibility_distances.csv') %>% 
  dplyr::mutate(threshold = paste0(threshold, 'min'))

acc_base <- readr::read_csv('results/02_housing_acc_segreg/table/accessibility_base.csv') 

# housing 
private_df <- sf::st_read('data/housing/private/CEM_LancaRes_SP_2000_2013.shp', quiet= T) %>% 
  sf::st_drop_geometry()

# Appendix A: DECISIONS ----
# appendix: bar plot of distribution of cells by scale of accessibility ----

p <- accessibility %>% 
  dplyr::filter(threshold!='5min', year==2017) %>% 
  dplyr::mutate(range = as.numeric(as.character(factor(cut(perc,
                                                           breaks = seq(0,1,1/quant),
                                                           label = (1:quant)-.5,
                                                           include.lowest = T,
                                                           right = F))))) %>% 
  dplyr::rename(id = origin) %>% 
  dplyr::right_join(sf::st_drop_geometry(grid)) %>% 
  tidyr::complete(id, fill=list(range = 0.5, perc = 0, acc = 0)) %>% 
  dplyr::filter(!is.na(threshold)) %>% 
  dplyr::mutate(threshold = factor(threshold,
                                   levels = c('15min', '30min', '60min', '120min'))) %>% 
  dplyr::mutate(threshold = factor(threshold,
                                   levels = c('120min', '60min', '30min', '15min')))%>% 
  dplyr::group_by(range, threshold) %>% 
  dplyr::summarise(ct = dplyr::n()) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(threshold) %>% 
  dplyr::mutate(perc = ct/sum(ct, na.rm=T)) %>% 
  ggplot() + 
  geom_bar(aes(x = threshold, y = perc, fill = range), position = 'stack', stat = 'identity')+
  scale_fill_stepsn('% do total de empregos',
                    colors = colOrange(quant),
                    values = scales::rescale((1:quant)-.5, 
                                             from = c(0,quant)),
                    breaks = 0:quant,
                    labels = seq(0,100, 20),
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
  labs(y= '% de células', x = 'limite de tempo') +
  theme_chart +
  theme(plot.margin = margin(2,7.5,2,2,'mm'))

p
size <- dplyr::filter(map_dim, map=='chart')
ggsave(paste0('latex/dissertacao_bms_vfull/figuras/apendice/barplot_acc.png'), 
       p, dpi = 500, width = size$width, height = size$height, units = "mm", type = 'cairo-png')

# Appendix B: HOUSING LIMITS ----

h_used <- private_df %>% 
  dplyr::filter(ANO_LAN %in% 2002:2013, HOTEL==0, FLAT==0) %>% 
  dplyr::mutate(AR_UT_UNID = as.numeric(AR_UT_UNID),
                PC_TT_ATU = as.numeric(PC_TT_ATU),
                classe = dplyr::case_when(AR_UT_UNID <= 50 & BANH_UNID == 1 & 
                                            GAR_UNID <= 1 & PC_TT_ATU <= 68429.16 ~ 'lower', #88320.25  fase 1 68429.16
                                          AR_UT_UNID <= 70 & 
                                            BANH_UNID <= 2 & GAR_UNID <= 1 & 
                                            PC_TT_ATU <= 171072.90 ~ 'medium', # 273090.90 fase 1 171072.90
                                          TRUE ~ 'higher'),
                classe = factor(classe, 
                                levels = c('lower', 'medium', 'higher'),
                                labels = c('HIS', 'HMP', 'alto padrão')),
                ano = as.numeric(as.character(ANO_LAN)),
                caso = 'usado')

h_a <-  private_df %>% 
  dplyr::filter(ANO_LAN %in% 2002:2013, HOTEL==0, FLAT==0) %>% 
  dplyr::mutate(AR_UT_UNID = as.numeric(AR_UT_UNID),
                PC_TT_ATU = as.numeric(PC_TT_ATU),
                classe = dplyr::case_when(AR_UT_UNID <= 50 & BANH_UNID == 1 & 
                                            GAR_UNID <= 1 & PC_TT_ATU <= 250000 ~ 'lower', #88320.25  fase 1 68429.16
                                          AR_UT_UNID <= 70 & 
                                            BANH_UNID <= 2 & GAR_UNID <= 1 & 
                                            PC_TT_ATU <= 650000 ~ 'medium', # 273090.90 fase 1 171072.90
                                          TRUE ~ 'higher'),
                classe = factor(classe, 
                                levels = c('lower', 'medium', 'higher'),
                                labels = c('HIS', 'HMP', 'alto padrão')),
                ano = as.numeric(as.character(ANO_LAN)),
                caso = 'caso a')

h_b <-  private_df %>% 
  dplyr::filter(ANO_LAN %in% 2002:2013, HOTEL==0, FLAT==0) %>% 
  dplyr::mutate(AR_UT_UNID = as.numeric(AR_UT_UNID),
                PC_TT_ATU = as.numeric(PC_TT_ATU),
                classe = dplyr::case_when(AR_UT_UNID <= 50 & BANH_UNID == 1 & 
                                            GAR_UNID <= 1 & PC_TT_ATU <= 125000 ~ 'lower', #88320.25  fase 1 68429.16
                                          AR_UT_UNID <= 70 & 
                                            BANH_UNID <= 2 & GAR_UNID <= 1 & 
                                            PC_TT_ATU <= 325000 ~ 'medium', # 273090.90 fase 1 171072.90
                                          TRUE ~ 'higher'),
                classe = factor(classe, 
                                levels = c('lower', 'medium', 'higher'),
                                labels = c('HIS', 'HMP', 'alto padrão')),
                ano = as.numeric(as.character(ANO_LAN)),
                caso = 'caso b')

h_c <-  private_df %>% 
  dplyr::filter(ANO_LAN %in% 2002:2013, HOTEL==0, FLAT==0) %>% 
  dplyr::mutate(AR_UT_UNID = as.numeric(AR_UT_UNID),
                PC_TT_ATU = as.numeric(PC_TT_ATU),
                classe = dplyr::case_when(AR_UT_UNID <= 50 & BANH_UNID == 1 & 
                                            GAR_UNID <= 1 & PC_TT_ATU <= 85000 ~ 'lower', #88320.25  fase 1 68429.16
                                          AR_UT_UNID <= 70 & 
                                            BANH_UNID <= 2 & GAR_UNID <= 1 & 
                                            PC_TT_ATU <= 275000 ~ 'medium', # 273090.90 fase 1 171072.90
                                          TRUE ~ 'higher'),
                classe = factor(classe, 
                                levels = c('lower', 'medium', 'higher'),
                                labels = c('HIS', 'HMP', 'alto padrão')),
                ano = as.numeric(as.character(ANO_LAN)),
                caso = 'caso c')

p <- h_used %>% 
  dplyr::bind_rows(h_a) %>% 
  dplyr::bind_rows(h_b) %>% 
  dplyr::bind_rows(h_c) %>% 
  dplyr::group_by(ANO_LAN, classe, caso) %>%
  dplyr::summarize(qtde = sum(TT_UNID)) %>% 
  dplyr::ungroup() %>%
  dplyr::group_by(ANO_LAN, caso) %>% 
  dplyr::mutate(prop = qtde/ sum(qtde, na.rm=T))%>%
  ggplot() +
  geom_area(aes(x = ANO_LAN, y = prop, 
                fill = classe, group = classe), 
            stat = 'identity', position = 'stack')  +
  scale_fill_manual('classe',
                    breaks = c('HIS', 
                               'HMP',
                               'alto padrão'),
                    values = colOrange(5)[c(1,3,5)],
                    labels = c('HIS', 
                               'HMP',
                               'alto padrão'),
                    drop = F) +
  scale_y_continuous(labels = scales::percent)+
  labs(y = '% do total de UH',
       x = 'ano de lançamento') +
  coord_cartesian(ylim =  c(0,1), expand = F) +
  facet_wrap(~caso, nrow = 2)+
  theme_chart +
  theme(plot.margin = margin(2,7.5,2,2,'mm'),
        panel.spacing.x = unit(7.5, 'mm'),
        axis.text.x = element_text(angle = 30))

p

size <- dplyr::filter(map_dim, map=='chart')
ggsave('latex/dissertacao_bms_vfull/figuras/apendice/area_types_mean.png',
       dpi = 500, type = 'cairo-png', width = size$width, height = size$width, units = 'mm')

# Appendix B: ACCESSIBILITY ----

# appendix: accessibility - percentile scale ----

# treating data
df_acc <- purrr::map_df(c(2010,2014), function(y){
  
  a <- dplyr::filter(accessibility, year == y, threshold == '60min')
  
  b <- quantile(a$perc, probs = seq(0,1,.2))
  
  a %>% 
    dplyr::mutate(range = as.numeric(as.character(factor(cut(perc,
                                                             breaks = b,
                                                             label = (1:quant)-.5,
                                                             include.lowest = T,
                                                             right = F))))) %>% 
    dplyr::rename(id = origin) %>% 
    dplyr::right_join(sf::st_drop_geometry(grid)) %>% 
    tidyr::complete(id, fill=list(range = 0.5, perc = 0, acc = 0)) %>% 
    dplyr::filter(!is.na(threshold)) 
  
})

# processing data to plot accessibility maps
plot_acc <- df_acc %>% 
  dplyr::left_join(grid) %>% 
  sf::st_as_sf() %>% 
  dplyr::group_by(year, range) %>% 
  dplyr::summarise() %>% 
  dplyr::ungroup() 

# data of differences
dif <- df_acc %>% 
  dplyr::select(id, year, range) %>% 
  tidyr::pivot_wider(names_from = year, names_prefix = 'y_', values_from = range) %>% 
  dplyr::mutate(dif = y_2014 - y_2010,
                diff = as.numeric(as.character(cut(dif,
                                                   breaks = c(-Inf, -2, -1,1,2,Inf),
                                                   labels= 1:5))),
                year = 'diferença') %>% 
  dplyr::left_join(grid) %>% 
  sf::st_as_sf() %>% 
  dplyr::group_by(year, diff) %>% 
  dplyr::summarise() %>% 
  dplyr::ungroup() 

breaks <- c(-Inf, -2, -1,1,2,Inf)
lb <- c(expression(''%<-%''), breaks[c(-1,-(quant+1))], expression(''%->%''))

t <- dplyr::filter(trilhos, year %in% 2010:2014)

p <- ggplot() +
  annotation_custom(ggbasemap, xmin=-Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
  geom_sf(data = plot_acc, aes(fill = range), color = NA) +
  scale_fill_stepsn('percentil',
                    colors = colOrange(quant),
                    values = scales::rescale((1:quant)-.5, 
                                             from = c(0,quant)),
                    breaks = 0:quant,
                    labels = 0:5,
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
  geom_sf(data=t, aes(color = "trilhos"),
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
ggsave(paste0('latex/dissertacao_bms_vfull/figuras/apendice/map_acc_percentile_2010_2014.png'), 
       p, dpi = 500, width = size$width, height = size$height, units = "mm", type='cairo-png')

# appendix: map of jobs 2002 - 2014 ----

op <- dplyr::filter(opportunities_org, year %in% 2000:2014)

max <- max(op$job, na.rm=T)
max <- dplyr::filter(op, job == max) %>%  .$year

brks <- op %>% 
  dplyr::filter(year == max, job!=0) %>% 
  .$job %>% 
  quantile(., probs = seq(0,1,1/quant))

brks[1] <- 0

plot <- op %>% 
  dplyr::mutate(range = cut(job, 
                            breaks = brks,
                            include.lowest = T,
                            right = F)) %>% 
  dplyr::group_by(range, year) %>% 
  dplyr::summarise() %>% 
  dplyr::ungroup()

p_jobs <- ggbase_brk(plot, 'empregos (10³)')

size <- dplyr::filter(map_dim, map=='4')
ggsave(paste0('latex/dissertacao_bms_vfull/figuras/apendice/map_jobs.png'), 
       p_jobs, dpi = 500, width = size$width, height = size$height, units = "mm", type = 'cairo-png')

# appendix: bar plot of jobs ----

labs <- as.numeric(stringr::str_replace_all(levels(plot$range), '\\[|\\)|\\]|.*,',''))
labs <- c(0, formatC(as.numeric(labs[1:4]),format='d',big.mark = '.', decimal.mark = ','), expression(''%->%''))
p <- op %>% 
  dplyr::mutate(range = as.numeric(cut(job, 
                            breaks = brks,
                            include.lowest = T,
                            right = F)) - 0.5,
                year = factor(year,
                              levels = c(2014, 2010, 2007, 2002)))%>% 
  dplyr::group_by(range, year) %>% 
  dplyr::summarise(ct = dplyr::n()) %>% 
  dplyr::ungroup() %>% 
  dplyr::filter(!is.na(range)) %>% 
  dplyr::group_by(year) %>% 
  dplyr::mutate(perc = ct/sum(ct, na.rm=T)) %>% 
  ggplot() + 
  geom_bar(aes(x = year, y = perc, fill = range), position = 'stack', stat = 'identity')+
  scale_fill_stepsn('empregos (10³)',
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
  scale_y_continuous(labels = scales::percent, breaks = seq(0,1,.2))+
  coord_flip(expand = F) +
  labs(y= '% de células', x = 'ano') +
  theme_chart +
  theme(plot.margin = margin(2,7.5,2,2,'mm'))

size <- dplyr::filter(map_dim, map=='chart')
ggsave(paste0('latex/dissertacao_bms_vfull/figuras/apendice/barplot_jobs.png'), 
       p, dpi = 500, width = size$width, height = size$height, units = "mm", type = 'cairo-png')

# appendix: accessibility 2002 - 2014 - relative scale ----

# treating data
df_acc_org <- accessibility %>% 
  dplyr::filter(year!=2017, threshold == '60min') %>% 
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
plot_acc_org <- df_acc_org %>% 
  dplyr::left_join(grid) %>% 
  sf::st_as_sf() %>% 
  dplyr::group_by(year, range) %>% 
  dplyr::summarise() %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(year = factor(year,
                              levels = c(2003, 2007, 2010, 2014, 
                                         '2003 - 2007', '2007 - 2010', '2010 - 2014')))

# data for difference map
dif_org <- df_acc_org %>% 
  dplyr::select(id, year, perc) %>% 
  tidyr::pivot_wider(names_from = year, names_prefix = 'y_', values_from = perc) %>% 
  dplyr::mutate(`2010 - 2014`= y_2014/y_2010 - 1,
                `2003 - 2007`= y_2007/y_2003 - 1,
                `2007 - 2010`= y_2010/y_2007 - 1) %>% 
  tidyr::pivot_longer(cols = c('2010 - 2014', '2003 - 2007', '2007 - 2010'),
                      names_to = 'year',
                      values_to = 'dif')%>% 
  dplyr::mutate(diff = as.numeric(as.character(cut(dif,
                                                   breaks = c(-Inf, -0.5, -.1,.1,.5,Inf),
                                                   labels= 1:5,
                                                   right = F,
                                                   include.lowest = T))) - .5,
                year = factor(year,
                              levels = c(2003, 2007, 2010, 2014, 
                                         '2003 - 2007', '2007 - 2010', '2010 - 2014'))) %>% 
  dplyr::left_join(grid) %>% 
  sf::st_as_sf() %>% 
  dplyr::group_by(year, diff) %>% 
  dplyr::summarise() %>% 
  dplyr::ungroup() 

breaks <- c(-Inf, -0.5, -.1,.1,.5,Inf)
lb <- c(expression(''%<-%''), scales::percent(breaks[c(-1,-(quant+1))]), expression(''%->%''))

t <- trilhos %>% 
  dplyr::filter(year != 2017) %>% 
  dplyr::mutate(year = factor(year,
                              levels = c(2003, 2007, 2010, 2014, 
                                         '2003 - 2007', '2007 - 2010', '2010 - 2014')))

p <- ggplot() +
  annotation_custom(ggbasemap, xmin=-Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
  geom_sf(data = plot_acc_org, aes(fill = range), color = NA) +
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
  geom_sf(data = dif_org, aes(fill = diff), color = NA) +
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
  geom_sf(data=t, aes(color = "trilhos"),
          size = line.size/200, show.legend = "line") +
  labs(caption = bg$caption) +
  scale_color_manual(breaks = "trilhos",
                     values = c("trilhos" = "grey40"),
                     guide = guide_legend(title= "", 
                                          order = 1,
                                          title.position = "top")) +
  theme_bw() +
  theme_map +
  coord_lim +
  facet_wrap(. ~ year, nrow = 2, ncol = 4) 

size <- dplyr::filter(map_dim, map=='4')
ggsave(paste0('latex/dissertacao_bms_vfull/figuras/apendice/map_acc_2002_2014.png'), 
       p, dpi = 500, width = size$width, height = size$width*.9, units = "mm", type='cairo-png')

# appendix: bar plot of cells in each class of accessibility ----

p <- accessibility %>% 
  dplyr::filter(threshold== '60min', year!=2017) %>% 
  dplyr::mutate(range = as.numeric(as.character(factor(cut(perc,
                                                           breaks = seq(0,1,1/quant),
                                                           label = (1:quant)-.5,
                                                           include.lowest = T,
                                                           right = F)))),
                year = factor(year,
                              levels = c(2014, 2010, 2007, 2003))) %>% 
  dplyr::group_by(year, range) %>% 
  dplyr::summarise(n = dplyr::n()) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(year) %>% 
  dplyr::mutate(perc = n/sum(n)) %>% 
  ggplot() + 
  geom_bar(aes(x = factor(year), y = perc, fill = range), position = 'stack', stat = 'identity') +
  scale_fill_stepsn('% do total de empregos',
                    colors = colOrange(quant),
                    values = scales::rescale((1:quant)-.5, 
                                             from = c(0,quant)),
                    breaks = 0:quant,
                    labels = seq(0,100,20),
                    limits = c(0,quant),
                    na.value=colOrange(10)[1],
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
  labs(y= '% de células', x = 'ano') +
  theme_chart +
  theme(plot.margin = margin(2,7.5,2,2,'mm'))

size <- dplyr::filter(map_dim, map=='chart')
ggsave(paste0('latex/dissertacao_bms_vfull/figuras/apendice/barplot_acc_2010_2014.png'), 
       p, dpi = 500, width = size$width, height = size$height, units = "mm", type='cairo-png')

# appendix: accessibility 2002 - 2014 ops of 2014 - relative scale ----

# treating data
df_acc_org <- acc_base %>% 
  dplyr::filter(year!=2017, threshold == 60) %>% 
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
plot_acc_org <- df_acc_org %>% 
  dplyr::left_join(grid) %>% 
  sf::st_as_sf() %>% 
  dplyr::group_by(year, range) %>% 
  dplyr::summarise() %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(year = factor(year,
                              levels = c(2003, 2007, 2010, 2014, 
                                         '2003 - 2007', '2007 - 2010', '2010 - 2014')))

# data for difference map
dif_org <- df_acc_org %>% 
  dplyr::select(id, year, perc) %>% 
  tidyr::pivot_wider(names_from = year, names_prefix = 'y_', values_from = perc) %>% 
  dplyr::mutate(`2010 - 2014`= y_2014/y_2010 - 1,
                `2003 - 2007`= y_2007/y_2003 - 1,
                `2007 - 2010`= y_2010/y_2007 - 1) %>% 
  tidyr::pivot_longer(cols = c('2010 - 2014', '2003 - 2007', '2007 - 2010'),
                      names_to = 'year',
                      values_to = 'dif')%>% 
  dplyr::mutate(diff = as.numeric(as.character(cut(dif,
                                                   breaks = c(-Inf, -0.5, -.1,.1,.5,Inf),
                                                   labels= 1:5,
                                                   right = F,
                                                   include.lowest = T))) - .5,
                year = factor(year,
                              levels = c(2003, 2007, 2010, 2014, 
                                         '2003 - 2007', '2007 - 2010', '2010 - 2014'))) %>% 
  dplyr::left_join(grid) %>% 
  sf::st_as_sf() %>% 
  dplyr::group_by(year, diff) %>% 
  dplyr::summarise() %>% 
  dplyr::ungroup() 

breaks <- c(-Inf, -0.5, -.1,.1,.5,Inf)
lb <- c(expression(''%<-%''), scales::percent(breaks[c(-1,-(quant+1))]), expression(''%->%''))

p <- ggplot() +
  annotation_custom(ggbasemap, xmin=-Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
  geom_sf(data = plot_acc_org, aes(fill = range), color = NA) +
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
  geom_sf(data = dif_org, aes(fill = diff), color = NA) +
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
  theme_bw() +
  theme_map +
  coord_lim +
  facet_wrap(. ~ year, nrow = 2, ncol = 4) 

size <- dplyr::filter(map_dim, map=='4')
ggsave(paste0('latex/dissertacao_bms_vfull/figuras/apendice/map_acc_2002_2014_same_ops.png'), 
       p, dpi = 500, width = size$width, height = size$width*.9, units = "mm", type='cairo-png')

# appendix: bar plot of cells in each class of accessibility ----

p <- acc_base %>% 
  dplyr::filter(threshold== 60, year!=2017) %>% 
  dplyr::mutate(range = as.numeric(as.character(factor(cut(perc,
                                                           breaks = seq(0,1,1/quant),
                                                           label = (1:quant)-.5,
                                                           include.lowest = T,
                                                           right = F)))),
                year = factor(year,
                              levels = c(2014, 2010, 2007, 2003))) %>% 
  dplyr::group_by(year, range) %>% 
  dplyr::summarise(n = dplyr::n()) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(year) %>% 
  dplyr::mutate(perc = n/sum(n)) %>% 
  ggplot() + 
  geom_bar(aes(x = factor(year), y = perc, fill = range), position = 'stack', stat = 'identity') +
  scale_fill_stepsn('% do total de empregos',
                    colors = colOrange(quant),
                    values = scales::rescale((1:quant)-.5, 
                                             from = c(0,quant)),
                    breaks = 0:quant,
                    labels = seq(0,100,20),
                    limits = c(0,quant),
                    na.value=colOrange(10)[1],
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
  labs(y= '% de células', x = 'ano') +
  theme_chart +
  theme(plot.margin = margin(2,7.5,2,2,'mm'))

size <- dplyr::filter(map_dim, map=='chart')
ggsave(paste0('latex/dissertacao_bms_vfull/figuras/apendice/barplot_acc_2010_2014_same_ops.png'), 
       p, dpi = 500, width = size$width, height = size$height, units = "mm", type='cairo-png')


