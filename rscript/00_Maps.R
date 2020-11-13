# Functions ----

# color ramp
colOrange <- colorRampPalette(c('#f4cf56', '#dc7c20', '#ca1d45', '#701A92', '#0b0743'))
#colOrange <- colorRampPalette(c('#efef6b', '#ea985d', '#e81e35', '#621d84', '#011835'))
colRedBlue <- colorRampPalette(c("#ca1d45", "#d57f93", "#E1E1E1", "#51838d", "#095563"))
#colRedBlue <- colorRampPalette(c("#ca1d46", "#d57f93", "#E1E1E1", "#8cb9c3", "#3891a6"))
colCat <- c("#095563", "#ca1d45", "#500b6b", "#d0da1f", "#dc7c20")
#colCat <- c("#3891a6", "#A51D5C", "#E95B49", "#311A5C", "#ECC364")
colBivariate <- c('11' = '#e1e1e1', '21' = '#51838d','31' = '#095563',
                  '12' = '#d57f93', '22' = '#5d4a68','32' = '#0a2e53',
                  '13' = '#ca1d45', '23' = '#6a1244','33' = '#0b0743')
#colBivariate <- c('11' = '#e1e1e1', '21' = '#8cb9c3','31' = '#3891a6',
#                  '12' = '#d57f93', '22' = '#786980','32' = '#1c546d',
#                  '13' = '#ca1d46', '23' = '#651a3d','33' = '#011835')

#' functions bellow were created by clauswilke
#' suggested at https://github.com/tidyverse/ggplot2/issues/2673#issuecomment-402878574
#discrete_gradient_pal <- function(colours, bins = 5) {
#    ramp <- scales::colour_ramp(colours)
#    
#    function(x) {
#        if (length(x) == 0) return(character())
#        
#        i <- floor(x * bins)
#        i <- ifelse(i > bins-1, bins-1, i)
#        ramp(i/(bins-1))
#    }
#}

#scale_colour_bin <- function(..., colours, bins = 5, 
#                             na.value = "grey50", 
#                             guide = "colourbar", 
#                             aesthetics = "colour", 
#                             colors)  {
#    colours <- if (missing(colours)) 
#        colors
#    else colours
#    continuous_scale(
#        aesthetics,
#        "discrete_gradient",
#        discrete_gradient_pal(colours, bins),
#        na.value = na.value,
#        guide = guide,
#        ...
#    )
#}

# Reading files ----

prjutm <- "+proj=utm +zone=23 +south +datum=WGS84 +units=m +no_defs"
crsutm <- 32723
crsutm_sirgas <- 31983
prjdegrees <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
crsdegrees <- 4326

# base shp files
sp <- sf::st_read("data/shp/MSP.shp", quiet=T, crs=crsutm_sirgas) %>% 
    sf::st_transform(prjdegrees)
muni <- sf::st_read("data/shp/Outros_muni-MSP.shp", quiet=T, crs=crsutm) %>% 
    sf::st_transform(prjdegrees) 
massa <- sf::st_read("data/shp/Massa_dagua_MSP.shp", quiet=T, crs = crsutm_sirgas) %>% 
    sf::st_transform(prjdegrees) %>% 
    dplyr::group_by() %>% 
    dplyr::summarise()
rios <- sf::st_read("data/shp/Rio_MSP.shp", quiet=T, crs=crsutm_sirgas) %>% 
    sf::st_transform(prjdegrees) %>% 
    dplyr::group_by() %>% 
    dplyr::summarise()
verde <- sf::st_read("data/shp/Verde_MSP.shp", quiet=T, crs=crsutm_sirgas) %>% 
    sf::st_transform(prjdegrees)

#trilhos
trilhos<- sf::st_read("data/shp/rails/trilhos_utm.shp", quiet=T, crs = crsutm_sirgas) %>% 
    dplyr::mutate(year = ifelse(ano == 2002, 2003, ano)) %>% 
    sf::st_transform(prjdegrees)

# grid
grid <- sf::st_read('data/shp/grid_hex_MSP_short.shp', quiet = T)%>% 
    sf::st_transform(prjdegrees)

# Defining colors ----
colWater <- "#CFCED3" 
colGreen <-  "#E2E7E1" 

# size pattern  ----
base <- 210 # width of images
# size of font and line proportional to height of figure
font.size <- base/ggplot2::.pt*1.414286 # multiplied by the relation of a4 height by width
line.size <- base/ggplot2::.stroke*1.414286

coord_lim <- coord_sf(ylim = c(7346293.52,7413300.44), 
                      xlim = c(314223.75, 360463.96),
                      crs = crsutm) 

coord_lim_short <- coord_sf(xlim = c(-46.826081, -46.365377), 
                            ylim = c(-23.716492, -23.384091),
                            crs = crsdegrees)

# Theme charts ----
theme_chart <- theme_bw() +
    theme(panel.border = element_blank(),
          panel.spacing = unit(2, 'mm'),
          # axis
          axis.line = element_line(color='grey30'),
          axis.ticks = element_line(color='grey30'),
          axis.text = element_text(size = font.size*0.1),
          axis.title = element_text(size = font.size*0.12),
          # legend
          legend.position = 'bottom',
          legend.text = element_text(size=font.size*0.1),
          legend.title = element_text(size=font.size*0.12),
          legend.spacing.x = unit(.01,"cm"),
          # strip
          strip.background = element_blank(),
          strip.text = element_text(size = font.size*0.12),
          # margin
          plot.margin = margin(2, 2, 2, 2, 'mm'))

theme_map <- theme(panel.border = element_rect(color = "grey85"),
                   # axis
                   axis.line = element_blank(),
                   axis.ticks = element_blank(),
                   axis.text = element_blank(),
                   axis.title = element_blank(),
                   # facet grid
                   panel.grid = element_line(color=colWater),
                   panel.spacing = unit(2,'mm'),
                   # legend
                   legend.position = "bottom",
                   legend.box = "horizontal",
                   legend.direction = "horizontal",
                   legend.spacing = unit(base*0.1, "mm"),
                   legend.text = element_text(size=font.size*0.1),
                   legend.title = element_text(size=font.size*0.12),
                   # facet
                   strip.background = element_blank(), 
                   strip.text = element_text(size = font.size*0.12),
                   # background
                   panel.background = element_rect(fill=colWater),
                   plot.background = element_blank(),
                   # caption
                   plot.caption = element_text(size = font.size*0.08,
                                               hjust = 1),
                   # margin
                   plot.margin = margin(2, 2, 2, 2, 'mm'))

theme_map_2 <- theme_map +
    theme(plot.caption = element_text(hjust = -.95),
          plot.caption.position = 'plot',
          plot.margin = margin(2, 54, 2, 54, 'mm'))

theme_map_3 <- theme_map +
    theme(plot.margin = margin(2, 24, 2, 24, 'mm'),
          plot.caption = element_text(hjust = 1.65),
          plot.caption.position = 'plot',
          panel.spacing = unit(2,'mm'))

# Background options ----

## This part downloads the basemap and saves as png 
## ggbasemap 
bg_options <- read.csv2('background_map_tiles.csv', stringsAsFactors = F)
basemap_option <- 'esri-light-gray' # basemap
bg <- bg_options[bg_options$option==basemap_option,]
#p <- ggplot() +
#    ggspatial::annotation_map_tile(type = bg$link, 
#                                   zoom = 12,
#                                   quiet = T,
#                                   progress = 'text')+ 
#    coord_lim_short +
#    theme_void()
#ggsave('test.png', plot = p, dpi=500, width = base, height = base-45, units = 'mm')

basemap <- png::readPNG('data/basemap_esri_light_gray_12.png')
ggbasemap <- grid::rasterGrob(basemap, interpolate = T)
basemap <- png::readPNG('data/basemap_short_esri_light_gray_12.png')
ggbasemap_short <- grid::rasterGrob(basemap, interpolate = T)
# Maps bases ----

ggbase_isolation <- function(plot, title, matrix_type, bandwidth){
    plot %>% 
        ggplot() +
        annotation_custom(ggbasemap, xmin=-Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
        geom_sf(aes(fill = range), color = NA) +
        geom_sf(data = massa, fill = colWater, color = colWater, size = line.size/500)  +
        geom_sf(data=rios, color=colWater, size=line.size/250) + 
        geom_sf(data=verde, fill=colGreen, color="transparent") +
        geom_sf(data=muni, fill = 'grey40', color = 'transparent', alpha = 0.25) +
        geom_sf(data=trilhos, aes(color = "trilhos"),
                size = line.size/200, show.legend = "line") +
        labs(caption = paste0(bg$caption,'\n',
                              'Legenda baseada na distribuição da banda de ', bandwidth, '.')) +
        scale_fill_stepsn(title,
                          colors = colOrange(quant),
                          values = scales::rescale((1:quant)-.5, 
                                                   from = c(0,quant)),
                          breaks = 0:quant,
                          labels = c(expression(''%<-%''), rep('',quant-1), expression(''%->%'')),
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
        theme_map +
        coord_lim
}

ggbase_perc <- function(plot, title){
    plot %>% 
        ggplot() +
        annotation_custom(ggbasemap, xmin=-Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
        geom_sf(aes(fill = range), color = NA) +
        geom_sf(data = massa, fill = colWater, color = colWater, size = line.size/500)  +
        geom_sf(data=rios, color=colWater, size=line.size/250) + 
        geom_sf(data=verde, fill=colGreen, color="transparent") +
        geom_sf(data=muni, fill = 'grey40', color = 'transparent', alpha = 0.25) +
        geom_sf(data=trilhos, aes(color = "trilhos"),
                size = line.size/200, show.legend = "line") +
        labs(caption = bg$caption) +
        scale_fill_stepsn(title,
                          colors = colOrange(quant),
                          values = scales::rescale((1:quant)-.5, 
                                                   from = c(0,quant)),
                          breaks = 0:quant,
                          labels = seq(0,100,100/quant),
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
        theme_map +
        coord_lim
}

ggbase_difference <- function(plot, title, breaks){
    lb <- c(expression(''%<-%''), breaks[c(-1,-(quant+1))], expression(''%->%''))
    plot %>% 
        ggplot() +
        annotation_custom(ggbasemap, xmin=-Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
        geom_sf(aes(fill = diff), color = NA) +
        geom_sf(data = massa, fill = colWater, color = colWater, size = line.size/500)  +
        geom_sf(data=rios, color=colWater, size=line.size/250) + 
        geom_sf(data=verde, fill=colGreen, color="transparent") +
        geom_sf(data=muni, fill = 'grey40', color = 'transparent', alpha = 0.25)+
        geom_sf(data=trilhos, aes(color = "trilhos"),
                size = line.size/200, show.legend = "line") +
        labs(caption = bg$caption) +
        scale_fill_stepsn(title,
                         colors = colRedBlue(quant),
                         breaks = 0:quant,
                         values = scales::rescale((1:quant)-.5, 
                                                  from = c(0,quant)),
                         labels = lb,
                         limits = c(0,quant),
                         expand = F,
                         right = T,
                         na.value=colRedBlue(quant)[3],
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
        theme_map +
        coord_lim
}

ggbase_brk <- function(plot, title){
    labs <- as.numeric(stringr::str_replace_all(levels(plot$range), '\\[|\\)|\\]|.*,',''))
    labs <- c(0, formatC(as.numeric(labs[1:4]),format='d',big.mark = '.', decimal.mark = ','), expression(''%->%''))
    trilhos <- dplyr::filter(trilhos, year %in% unique(plot$year))
    plot %>% 
        dplyr::mutate(range = as.numeric(range)-.5) %>% 
        ggplot() +
        annotation_custom(ggbasemap, xmin=-Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
        geom_sf(aes(fill = range), color = NA) +
        geom_sf(data = massa, fill = colWater, color = colWater, size = line.size/500)  +
        geom_sf(data=rios, color=colWater, size=line.size/250) + 
        geom_sf(data=verde, fill=colGreen, color="transparent") +
        geom_sf(data=muni, fill = 'grey40', color = 'transparent', alpha = 0.25) +
        geom_sf(data=trilhos, aes(color = "trilhos"),
                size = line.size/200, show.legend = "line") +
        labs(caption = bg$caption) +
        scale_fill_stepsn(title,
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
        facet_grid(.~year) +
        theme_bw() +
        theme_map +
        coord_lim
}

# map dimensions ----

map_dim <- data.frame(map = c(1, 2, 3, 4, 5, 8, 'chart','3_short', 'chart_short'),
                      width = base,
                      height = c(1.65, 0.9 ,0.65, 0.55, 0.46, 0.85, 0.45, 0.95, 0.45*.25)*base)
