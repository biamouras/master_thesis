#' Plots figure 02 - Map of ZEIS

# libraries ----
library(magrittr)
library(ggplot2)
library(egg)
base <- 210
quant <- 5
source('rscript/00_Maps.R')

# reading files ----

# zeis
zeis02_sf <- sf::st_read('data/shp/zones/zeis_2002_clean.shp', quiet = T) %>% 
  dplyr::mutate(zeis = as.numeric(stringr::str_replace_all(Layer, 'ZEIS\\_|\\_LIMITE','')),
                id = 1:dplyr::n()) %>% 
  dplyr::select(id, zeis) %>% 
  sf::st_transform(prjdegrees) %>% 
  dplyr::mutate(year = 'PDE 2002')
# map zeis ----

p <- ggplot()+
  annotation_custom(ggbasemap, xmin=-Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
  geom_sf(data = zeis02_sf, aes(fill = paste('zeis',zeis)), color = NA) +
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
  scale_fill_manual('', 
                    values = colOrange(5),
                    guide = guide_legend(order = 2,
                                         title.position = 'top',
                                         label.position = "bottom",
                                         barheight = base/45*.7,
                                         barwidth = base/3*.7,
                                         override.aes = list(shape = NA,
                                                             linetype = "blank"))) + 
  ggspatial::annotation_scale(location = 'br', 
                              bar_cols = c('grey15', 'grey99'), 
                              line_width = 0.3,
                              line_col = 'grey15',
                              text_col = 'grey15') +
  theme_bw() +
  theme_map +
  coord_lim +
  theme(plot.margin = margin(2, 54, 2, 54, 'mm'),
        plot.caption = element_text(hjust = -.95),
        plot.caption.position = 'plot',
        panel.spacing = unit(2,'mm'))


p
size <- dplyr::filter(map_dim, map=='4')
ggsave('latex/scs_zeis/figure/design/map_msp.png',
       plot = p, dpi = 500, type = 'cairo-png', width = size$width, height = size$height, units = 'mm')
