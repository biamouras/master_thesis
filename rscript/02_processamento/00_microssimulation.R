'''
Replicação do processamento de microssimulação espacial feito para a dissertação

Objetivo é de gerar uma população sintética de domicílios 
dividida entre grupos de renda familiar bruta
distribuída espacialmente em setores censitários
'''

# Bibliotecas ----
library(tidyverse)

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
  col_select = c('V0001', 'V0002','V0010', 
                 'V6532', 'V6530')) %>% 
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

# Tratamento dos dados da amostra ----

df_amostra_sp <- df_amostra %>% 
  filter(paste0(V0001,V0002) == '3550308') %>% 
  mutate(v_restritiva = cut(
    V6532,
    breaks = c(0, 1/8, 1/4, 1/2, 1, 2, 3, 5, 10, 2000)
  ))
