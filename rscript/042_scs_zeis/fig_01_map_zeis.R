#' Plots figure 02 - Map of ZEIS

# Library ----
library(tidyverse)
library(patchwork)
library(ggnewscale)
base <- 210 # width of images
quant <- 5
source('rscript/00_Maps_eng.R')

# reading files ----
state <- sf::read_sf('data/shp/SP_state.shp')
msp <- sf::read_sf('data/shp/MSP.shp')
# zeis
zeis02_sf <- sf::st_read('data/shp/zones/zeis_2002_clean.shp', quiet = T) %>% 
  dplyr::mutate(zeis = as.numeric(stringr::str_replace_all(Layer, 'ZEIS\\_|\\_LIMITE','')),
                id = 1:dplyr::n()) %>% 
  dplyr::select(id, zeis) %>% 
  sf::st_transform(prjdegrees) %>% 
  dplyr::mutate(year = 'PDE 2002')
# map zeis ----

# fig 01 - location of MSP ----
# br bounding (-78.9,-33.83,-33.56,5.33)
bb_sp <- data.frame(x=c(-53.11,-53.11,-44.16,-44.16),
                    y=c(-26.49504,-18.76496,-18.76496,-26.49504))
p_br <- ggplot() +
  ggspatial::annotation_map_tile(type = bg$link, 
                                 zoom = 4,
                                 quiet = T,
                                 progress = 'text')+ 
  geom_sf(data = state, fill = '#ABABAB', color= 'transparent')+
  geom_polygon(data = bb_sp, aes(x,y), color = 'grey40', fill = 'transparent', 
               lwd = line.size/200) +
  coord_sf(ylim = c(-33.83,5.33), 
           xlim = c(-78.9,-33.56),
           crs = crsdegrees) +
  theme_void()

# sp bounding (-53.11,-26.49504,-44.16,-18.76496)
#ylim = c(-46.826081, -46.365377), 
#xlim = c(-23.984431, -23.384091)
# bb_msp <- data.frame(y = c(-23.984431, -23.984431, -23.384091, -23.384091),
# x = c(-46.826081, -46.365377, -46.365377, -46.826081))

# short -46.826409,-23.795794,-46.36509,-23.357


bb_msp <- data.frame(y = c(-23.795794, -23.795794, -23.357, -23.357),
                     x = c(-46.826409, -46.36509, -46.36509, -46.826409))
p_sp <- ggplot() +
  ggspatial::annotation_map_tile(type = bg$link, 
                                 zoom = 7,
                                 quiet = T,
                                 progress = 'text')+ 
  geom_sf(data = state, color = '#ABABAB', fill = 'transparent', 
          lwd = line.size/200)+
  geom_sf(data = msp, fill = '#ABABAB', color = 'transparent') +
  geom_polygon(data = bb_msp, aes(x,y), color = 'grey40', fill = 'transparent', 
               lwd = line.size/200) +
  coord_sf(ylim = c(-26.49504,-18.76496),
           xlim = c(-53.11,-44.16),
           crs = crsdegrees) +
  theme_void()

p_msp <- ggplot() +
  ggspatial::annotation_map_tile(type = bg$link, 
                                 zoom = 12,
                                 quiet = T,
                                 progress = 'text')+ 
  geom_sf(data = zeis02_sf, aes(fill = paste('ZEIS',zeis)), color = NA) +
  scale_fill_manual('', 
                    values = colOrange(5)[1:4],
                    breaks = paste('ZEIS', 1:4),
                    guide = guide_legend(order = 2,
                                         title.position = 'top',
                                         label.position = "bottom",
                                         barheight = base/45*.7,
                                         barwidth = base/3*.7,
                                         override.aes = list(color = NA,
                                                             fill = colOrange(5)[1:4]))) + 
  new_scale_fill()+
  geom_sf(data = massa, aes(fill = 'reservoirs'),
          color = colWater, size = line.size/500)  +
  geom_sf(data=verde, aes(fill='green areas'), color="transparent") +
  scale_fill_manual('',
                    #breaks = c("reservoirs", 'green areas'),
                    values = c(colGreen,
                               colWater),
                    guide = guide_legend(order = 3,
                                         title.position = 'top',
                                         label.position = "bottom",
                                         barheight = base/45*.7,
                                         barwidth = base/3*.7,
                                         override.aes = list(colour = NA)))+
  geom_sf(data=rios, color=colWater, size=line.size/250) +
  geom_sf(data=muni, fill = 'grey40', color = 'transparent', alpha = 0.25) +
  geom_sf(data=dplyr::filter(trilhos, year == 2017), aes(color = "rails"),
          size = line.size/200, show.legend = "line") +
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
  labs(caption = bg$caption) +
  theme_bw() +
  theme_map +
  coord_sf(xlim=c(-46.826409, -46.36509),
           ylim=c(-23.795794, -23.357),
           crs = crsdegrees)

p_legend <- ggpubr::get_legend(p_msp)
p_msp <- p_msp +
  theme(legend.position = 'none',
        plot.caption = element_blank())

p <- ((p_br / p_sp) | p_msp )/ p_legend +
  plot_annotation(caption = bg$caption) +
  plot_layout(heights = c(1, .2),
              widths = c(.25,.5))

p

size <- dplyr::filter(map_dim, map=='3')
ggsave(paste0('latex/scs_zeis/figure/map_zeis.png'), 
       p, dpi = 500, width = size$width, height = size$height, units = "mm", type = 'cairo-png')
