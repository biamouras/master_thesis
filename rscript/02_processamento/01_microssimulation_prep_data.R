# Replicação do processamento de microssimulação espacial feito para a dissertação
# 
# Objetivo é de gerar uma população sintética de domicílios 
# dividida entre grupos de renda familiar bruta
# distribuída espacialmente em setores censitários


# Bibliotecas ----
library(tidyverse)
library(sf)
library(mipfp)

# funções e configuraçÃo geral  ----
options(scipen=999)

ajusta_decimal <- function(x){
  # algumas colunas da base da amostra têm casas decimais
  # a função vai tratar essas variáveis
  
  # seleciona a linha da variável no dicionário
  dic_var <- filter(dic_amostra, VAR == cur_column())
  
  novo_x <- paste0(
    str_sub(x, end = dic_var$INT),
    '.',
    str_sub(x, start = dic_var$INT+1, end = dic_var$INT+dic_var$DEC))
  
  novo_x <- as.numeric(novo_x)
  
  return(novo_x)
}

# de https://spatial-microsim-book.robinlovelace.net/smsimr.html#mipfp
int_trs <- function(x){
  # For generalisation purpose, x becomes a vector
  xv <- as.vector(x) # allows trs to work on matrices
  xint <- floor(xv) # integer part of the weight
  r <- xv - xint # decimal part of the weight
  def <- round(sum(r)) # the deficit population
  # the weights be 'topped up' (+ 1 applied)
  if(sum(r)>0){
    topup <- sample(length(x), size = def, prob = r)
    xint[topup] <- xint[topup] + 1  
  }
  dim(xint) <- dim(x)
  dimnames(xint) <- dimnames(x)
  xint
}

int_expand_array <- function(x){
  # Transform the array into a dataframe
  count_data <- as.data.frame.table(x)
  # Store the indices of categories for the final population
  indices <- rep(1:nrow(count_data), count_data$Freq)
  # Create the final individuals
  ind_data <- count_data[indices,]
  ind_data
}

# Carregamento de dados ----

## dados da amostra ----

# dicionário da amostra
dic_amostra <- readxl::read_xls(
  './data/population/census2010/Layout_microdados_Amostra.xls',
  sheet = 'DOMI',
  range = 'A2:L78'
) %>% 
  rename(
    POSICAO_INICIAL = `POSIÇÃO INICIAL`,
    POSICAO_FINAL = `POSIÇÃO FINAL`
  ) %>% 
  select(
    'VAR', 'NOME', 'POSICAO_INICIAL', 'POSICAO_FINAL',	'INT', 'DEC', 'TIPO'
  )

# microdados da amostra
df_amostra <- vroom::vroom_fwf(
  file = './data/population/census2010/Amostra_Domicilios_35_RMSP.txt',
  col_positions = vroom::fwf_positions(
    start = dic_amostra$POSICAO_INICIAL,
    end = dic_amostra$POSICAO_FINAL,
    col_names = dic_amostra$VAR
  ),
  col_select = c('V0001', 'V0002','V0010', 'V0300',
                 'V0011', 'V6532', 'V6530')) %>% 
  # ajustando casas decimais
  mutate(
    across(
      .cols = c('V0010', 'V6532', 'V6530'),
      .fns = ajusta_decimal
    )
  )

## dados do universo ----

df_universo <- read_delim(
  './data/population/census2010/DomicilioRenda_SP1.csv',
  delim = ';',
  na = 'X'
)

## relação entre áreas de ponderação (amostra) e setores censitários ----
relacao_areap_setor <- read_tsv(
  './data/population/census2010/Composicao das Areas de Ponderacao.txt',
  locale = locale(encoding = 'UTF-16')
) %>% #filtra são paulo
  filter(str_sub(Setor, 1, 7) == '3550308') 

# Tratamento dos dados da amostra ----

var_restritivas <- c('V014', paste0('V', str_pad(5:13, 3, pad = '0')))

df_amostra_sp <- df_amostra %>% 
  # filtra município de São Paulo
  # filtra domicílios que não responderam a renda
  filter(paste0(V0001,V0002) == '3550308',
         !is.na(V6532)) %>% 
  mutate(
    # cria relação com os mesmos nomes da universo
    v_restritiva = cut(
      V6532,
      breaks = c(-1, 0, 1/8, 1/4, 1/2, 1, 2, 3, 5, 10, 2000),
      labels = var_restritivas, 
    ),
    # variavel de interesse da microssimulação
    # renda domiciliar bruta em salários mínimos
    v_alvo = cut(
      V6530,
      breaks = c(-1, 3, 10, 4000),
      labels = c('G1', 'G2', 'G3')
    ))


# Tratamento dos dados do universo ----

df_universo_rest <- df_universo %>% 
  select(
    all_of(c(
      'Cod_setor',
      'V014', 
      paste0('V', str_pad(5:13, 3, pad = '0'))
    ))) %>% 
  column_to_rownames('Cod_setor') %>% 
  mutate(Cod_setor = rownames(.))

# IPF ----
areas_ponderacao <- unique(relacao_areap_setor$`Área de ponderação`)

setor_grupos <- map_df(areas_ponderacao, function(ap){
  message(ap)
  # seleciona os setores de uma AP no universo
  setores <- relacao_areap_setor %>% 
    filter(`Área de ponderação` == ap) %>% 
    .$Setor
  
  universo_ap <- df_universo_rest %>% 
    filter(Cod_setor %in% setores) %>% 
    select(-Cod_setor)
  
  # seleciona os indivíduos da AP
  amostra_ap <- df_amostra_sp %>% 
    filter(V0011 == ap) 
  
  ## estrutura os dados de entrada ----
  weight_init <- table(amostra_ap[,c('v_restritiva', 'v_alvo')])
  weight_zones <- rep(weight_init,
                     each = length(setores))
  weight_zones <- array(weight_zones,
                        dim = c(length(setores), length(var_restritivas), 3),
                        dimnames = c(list(setores), as.list(dimnames(weight_init))))
  
  # alvo de valores totais de cada setor
  target <- list(as.matrix(universo_ap))
  
  # ordem das variáveis restritivas
  descript <- list(1:2)
  
  ## implementação do Ipf ----
  weight_mipfp <- Ipfp(seed = weight_zones, 
                       target.list = descript, 
                       target.data = target,
                       na.target = T, tol = 1e-5)
  weight_mipfp <- weight_mipfp$x.hat
  
  ## inteirização por Truncate, Replicate, Sample ----
  walk(setores, function(setor){
    weight_mipfp[as.character(setor),,] <- int_trs(weight_mipfp[as.character(setor),,])
  })
  
  
  ## expansão e total da variável alvo por setor ----
  setores_mipfp <- as.data.frame.table(weight_mipfp) %>% 
    rename(peso = Freq,
           Cod_setor = Var1) %>% 
    group_by(Cod_setor, v_alvo) %>% 
    summarise(total = sum(peso), .groups = 'drop_last') %>% 
    pivot_wider(
      id_cols = 'Cod_setor',
      names_from = 'v_alvo',
      values_from = 'total',
      values_fill = 0
    )
  
  setores_mipfp
})

# total de domicílios
sum(setor_grupos[,c('G1', 'G2', 'G3')])

# Comparando com a microssimulação do mestrado ----

setor_grupos_mestrado <- read_csv('./data/population/population_micro_censustract.csv')
sum(setor_grupos_mestrado[,c('G1', 'G2', 'G3')], na.rm= T)


