# Replicação do processamento de microssimulação espacial feito para a dissertação
# 
# Objetivo é de gerar uma população sintética de domicílios 
# dividida entre grupos de renda familiar bruta
# distribuída espacialmente em setores censitários


# Bibliotecas ----
library(tidyverse)
library(sf)
library(mipfp)

# funções ----

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

todos_weight <- map(areas_ponderacao, function(ap){
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
  
  weight_init <-  amostra_ap %>% 
    # matriz inicial para cada zona
    pivot_wider(
      id_cols = 'V0011',
      names_from = 'v_restritiva',
      values_from = 'V0010', # considera o peso
      values_fn = sum
    ) %>% 
    select(-V0011) %>% 
    # agrega as matrizes n zonas vezes
    slice(rep(1, each = length(setores))) %>% 
    as.matrix(.)
  
  # define os nomes
  rownames(weight_init) <- setores
  
  # verifica se tem alguma variável faltando
  vars_faltando <- var_restritivas[!(var_restritivas %in% colnames(weight_init))]
  
  if(!identical(vars_faltando, character(0))){
    new_cols <- matrix(0, 
                       nrow = nrow(weight_init), 
                       ncol = length(vars_faltando),
                       dimnames = list(setores, vars_faltando))
    weight_init <- cbind(weight_init, new_cols)
  }
  
  # reordena as colunas
  weight_init <- weight_init[, var_restritivas]
  
  # alvo de valores totais de cada setor
  target <- list(as.matrix(universo_ap))
  
  # ordem das variáveis restritivas
  descript <- list(1:2)
  
  ## implementação do Ipf ----
  weight_mipfp <- Ipfp(seed = weight_init, 
                       target.list = descript, 
                       target.data = target,
                       na.target = T, tol = 1e-5)
  
  weight_mipfp$x.hat
})

