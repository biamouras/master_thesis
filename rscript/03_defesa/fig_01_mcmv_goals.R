#' Plots figure 01 and number analysis of MCMV
#' 
#' figure 01: bar chart with goals and contracted 

# Library ----
library(magrittr)
library(ggplot2)
library(ggnewscale)
base <- 210
quant <-  5
source('rscript/00_Maps.R')

# Reading files ----

mcmv <- readr::read_csv2('data/housing/mcmv/RESPOSTA_PEDIDO_2019_59017_000222.csv',
                        locale = readr::locale(encoding='latin1'))

names(mcmv) <- c('cod_ibge', 'pf/pj', 'uf', 'municipio', 'faixa', 'fase_mcmv',
                 'modalidade', 'uh_contratadas', 'uh_entregues', 'uh_concluidas',
                 'valor_tot', 'if', 'and_obra', 'empreendimento', 'endereco', 'construtora',
                 'cnpj', 'data', 'ano_contratacao')

meta <- tibble::tibble(fase_mcmv = c(.7, 1, 1.3, 1.7, 2, 2.3, 2.6625, 2.8875, 3.225),
                       faixa = c(rep(c('Faixa 1', 'Faixa 2', 'Faixa 3'), 2), c('Faixa 1', 'Faixa 1,5', 'Faixa 2 e 3')),
                       meta = c(400000, 400000, 200000, 1400000, 600000, 200000, 300000, 200000, 1190000),
                       w = c(rep(.27, 6), .2, .2, .425))

# number analysis of each municipality ----

rmsp <- mcmv %>% 
  dplyr::filter(municipio %in% c('Caieiras', 'Cajamar', 'Francisco Morato', 'Franco da Rocha', 'Mairiporã',
                                 'Arujá', 'Biritiba-Mirim', 'Ferraz de Vasconcelos', 'Guararema', 'Guarulhos', 
                                 'Itaquaquecetuba', 'Mogi das Cruzes', 'Poá', 'Salesópolis', 'Santa Isabel', 'Suzano',
                                 'Diadema', 'Mauá', 'Ribeirão Pires', 'Rio Grande da Serra', 'Santo André', 
                                 'São Bernardo do Campo', 'São Caetano do Sul', 'Cotia', 'Embu', 'Embu-Guaçu',
                                 'Itapecerica da Serra', 'Juquitiba', 'São Lourenço da Serra', 'Taboão da Serra',
                                 'Vargem Grande Paulista', 'Barueri', 'Carapicuíba', 'Itapevi', 'Jandira', 'Osasco',
                                 'Pirapora do Bom Jesus', 'Santana de Parnaíba')) %>% 
  dplyr::group_by(fase_mcmv, faixa) %>% 
  dplyr::summarise(uh_contr = sum(uh_contratadas, na.rm=T)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(perc = uh_contr/sum(uh_contr, na.rm=T)*100)

rj <- mcmv %>% 
  dplyr::filter(municipio %in% 'Rio de Janeiro') %>% 
  dplyr::group_by(fase_mcmv, faixa) %>% 
  dplyr::summarise(uh_contr = sum(uh_contratadas, na.rm=T)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(perc = uh_contr/sum(uh_contr, na.rm=T)*100)

bh <- mcmv %>% 
  dplyr::filter(municipio %in% 'Belo Horizonte') %>% 
  dplyr::group_by(fase_mcmv, faixa) %>% 
  dplyr::summarise(uh_contr = sum(uh_contratadas, na.rm=T)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(perc = uh_contr/sum(uh_contr, na.rm=T)*100)

sp <- mcmv %>% 
  dplyr::filter(municipio %in% 'São Paulo') %>% 
  dplyr::group_by(fase_mcmv, faixa) %>% 
  dplyr::summarise(uh_contr = sum(uh_contratadas, na.rm=T)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(perc = uh_contr/sum(uh_contr, na.rm=T)*100)

belem <- mcmv %>% 
  dplyr::filter(municipio %in% 'Belém') %>% 
  dplyr::group_by(fase_mcmv, faixa) %>% 
  dplyr::summarise(uh_contr = sum(uh_contratadas, na.rm=T)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(perc = uh_contr/sum(uh_contr, na.rm=T)*100)

camp <- mcmv %>% 
  dplyr::filter(municipio %in% 'Campinas') %>% 
  dplyr::group_by(fase_mcmv, faixa) %>% 
  dplyr::summarise(uh_contr = sum(uh_contratadas, na.rm=T)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(perc = uh_contr/sum(uh_contr, na.rm=T)*100)

fort <- mcmv %>% 
  dplyr::filter(municipio %in% 'Fortaleza') %>% 
  dplyr::group_by(fase_mcmv, faixa) %>% 
  dplyr::summarise(uh_contr = sum(uh_contratadas, na.rm=T)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(perc = uh_contr/sum(uh_contr, na.rm=T)*100)

# figure 01: bar chart with goals and contracted -----

meta %>% 
  ggplot() +
  geom_bar(aes(x=fase_mcmv, y = meta, width = w),
           stat = 'identity', 
           lty = 'dotted',
           orientation = 'x',
           lwd = line.size/200,
           fill = 'transparent',
           color = 'grey30')

mcmv %>% 
  dplyr::group_by(fase_mcmv, faixa) %>% 
  dplyr::summarise(uh_contratadas = sum(uh_contratadas, na.rm = T),
                   uh_entregues = sum(uh_entregues, na.rm = T)) %>% 
  dplyr::ungroup() %>% 
  ggplot() +
  # meta
  geom_bar(data = meta, aes(x = fase_mcmv, 
                            y = meta,  
                            group = faixa,
                            width = w),
           stat = 'identity', 
           orientation = 'x',
           lty = 'dotted',
           lwd = line.size/200,
           fill = 'transparent',
           color = 'grey30') +
  # contratadas
  geom_bar(aes(x=fase_mcmv, 
               y = uh_contratadas, 
               group = faixa,
               fill = faixa),
           width = 0.9,
           stat = 'identity', 
           position = 'dodge2',
           alpha=0.4) +
  # numero de meta
  geom_text(data = meta, aes(label = formatC(round(meta/1000, 2),format='d',big.mark = '.', decimal.mark = ','), 
                             x=fase_mcmv, 
                             y = meta + 40000,
                             group = faixa),
            position = position_dodge(0.9),
            vjust = 0,
            size = font.size*0.04) +
  # numero de contratadas
  geom_text(aes(label = formatC(round(uh_contratadas/1000, 2),format='d',big.mark = '.', decimal.mark = ','), 
                x=fase_mcmv, 
                y = 40000,
                group = faixa),
            position = position_dodge(0.9),
            vjust = 0,
            size = font.size*0.04,
            color = 'grey40') +
  # curva meta
  geom_curve(aes(x = 1.61,
                 xend = 1.3,
                 y = 1480000,
                 yend = 1290000),
             curvature = .4,
             size = line.size/200,
             arrow = arrow(length = unit(line.size/50, 'mm'))) +
  geom_text(aes(x = 1.3,
                y = 1250000,
                label = 'meta'),
            size = font.size*0.035) +
  # curva uh contratada
  geom_curve(aes(x = 1.7,
                 xend = 1.53,
                 y = 160000,
                 yend = 590000),
             color  = 'grey40',
             curvature = .3,
             size = line.size/200,
             angle = 110,
             arrow = arrow(length = unit(line.size/50, 'mm'))) +
  geom_text(aes(x = 1.3,
                y = 600000,
                label = 'UH contratadas'),
            size = font.size*0.035,
            color = 'grey40') +
  scale_fill_manual('faixa',
                    breaks = c('Faixa 1', 'Faixa 1,5', 'Faixa 2', 'Faixa 3'),
                    values = colOrange(5)[c(1, 2, 3, 4)],
                    labels = c('1', '1,5', '2', '3'),
                    guide = guide_legend(title.position = 'top')) +
  scale_y_continuous(limits = c(0,1600000), labels = function(l){l / 1000} ) +
  scale_x_continuous(labels = c('Fase 1', 'Fase 2', 'Fase 3'), breaks = c(1, 2, 3), limits = c(0.5, 3.5)) +
  theme_chart +
  coord_cartesian(expand = F) +
  labs(x = '', y = 'UH (k)') +
  theme(legend.position = 'bottom',
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.ticks.x = element_blank()) 

ggsave('latex/dissertacao_bms_vfull/figuras/revisao/barplot_mcmv_meta.png',
       dpi = 500, type = 'cairo-png', width = base, height = base*.45, units = 'mm')
