Microssimulação Espacial
================

- <a href="#preparação-do-ambiente-e-carregando-os-dados"
  id="toc-preparação-do-ambiente-e-carregando-os-dados">Preparação do
  ambiente e carregando os dados</a>
  - <a href="#dados-da-amostra" id="toc-dados-da-amostra">Dados da
    amostra</a>
  - <a href="#dados-do-universo" id="toc-dados-do-universo">Dados do
    universo</a>
- <a href="#tratamento-dos-dados" id="toc-tratamento-dos-dados">Tratamento
  dos dados</a>
- <a href="#aplicação-do-ipf" id="toc-aplicação-do-ipf">Aplicação do
  IPF</a>
- <a href="#comparando-com-a-microssimulação-do-mestrado"
  id="toc-comparando-com-a-microssimulação-do-mestrado">Comparando com a
  microssimulação do mestrado</a>
- <a href="#próximos-passos" id="toc-próximos-passos">Próximos passos</a>

Replicação do processamento de microssimulação espacial feito para a
[dissertação](https://doi.org/10.11606/D.3.2020.tde-31032021-143712).

O objetivo principal foi analisar o potencial das ZEIS sobre as
desigualdades de acesso ao trabalho, comparando com o programa Minha
Casa Minha Vida (PMCMV) e a produção habitacional do mercado formal em
São Paulo. Para tanto, foi necessário identificar quantas famílias deste
município que se enquadrariam nas faixas do PMCMV, a partir da renda
familiar mensal, e o seu local de residência.

Dado que os dados disponíveis pelos Resultados do Universo do Censo de
2010 não contém os domicílios por faixa de renda familiar mensal, foi
aplicada a microssimulação espacial para a geração de uma “população”
sintética de domicílios divida entre grupos de renda a partir dos
Resultados da Amostra.

O método aplicado foi o *Iterative Proportional Fitting* (IPF), o qual,
de modo iterativo, calcula o peso do indivíduo (no caso, domicílio) a
partir de uma variável em comum (chamada de restritiva) entre a amostra
e o censo para obter a população desagregada na escala maior (setor
censitário) com o valor da variável alvo presente apenas na escala
menor.

Os grupos de interesse são:

- Grupo 1: renda familiar até 3 salários-mínimos, que corresponde à
  faixa 1 do PMCMV e alvo da habitação de interesse social;
- Grupo 2: renda familiar entre 3 a 10 salários-mínimos, que corresponde
  às faixas 2 e 3 do PMCMV e alvo da habitação de mercado popular;
- Grupo 3: renda familiar acima de 10 salários-mínimos, que não eram
  atendidos pelo PMCMV

## Preparação do ambiente e carregando os dados

``` r
# bibliotecas necessárias
library(tidyverse)
library(sf)
library(mipfp)

# pasta do projeto
dir_proj <- here::here()
# pasta com os dados
dir_dados <- file.path(dir_proj, 'data', 'population', 'census2010')
```

Como mencionado, foram utilizados tanto os Resultados do Universo quanto
da Amostra do Censo 2010 e eles estão disponíveis no próprio [site do
IBGE](http://ftp.ibge.gov.br/Censos/Censo_Demografico_2010/).

### Dados da amostra

A base de dados da amostra está em txt e sem separação, sendo necessário
o uso do dicionário dos dados para a leitura do BD.

``` r
# leitura do dicionário da amostra
dic_amostra <- readxl::read_xls(
  file.path(dir_dados, 'Layout_microdados_Amostra.xls'),
  sheet = 'DOMI',
  range = 'A2:L78'
) %>% 
  # renomeia colunas
  rename(
    POSICAO_INICIAL = `POSIÇÃO INICIAL`,
    POSICAO_FINAL = `POSIÇÃO FINAL`
  ) %>% 
  # seleciona colunas de interesse
  select(
    'VAR', 'NOME', 'POSICAO_INICIAL', 'POSICAO_FINAL',  'INT', 'DEC', 'TIPO'
  )
```

A base da amostra tem algumas colunas que têm casas decimais (`DEC`)
além de números inteiros (`INT`). A função abaixo vai tratar essas
variáveis durante a leitura da base de dados.

``` r
ajusta_decimal <- function(x){
  
  # seleciona a linha da variável no dicionário
  dic_var <- filter(dic_amostra, VAR == cur_column())
  
  novo_x <- paste0(
    str_sub(x, end = dic_var$INT),
    '.',
    str_sub(x, start = dic_var$INT+1, end = dic_var$INT+dic_var$DEC))
  
  novo_x <- as.numeric(novo_x)
  
  return(novo_x)
}
```

``` r
# microdados da amostra
df_amostra <- vroom::vroom_fwf(
  file = file.path(dir_dados, 'Amostra_Domicilios_35_RMSP.txt'),
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
```

### Dados do universo

``` r
df_universo <- read_delim(
  file.path(dir_dados, 'DomicilioRenda_SP1.csv'),
  delim = ';',
  na = 'X'
)

# relação entre áreas de ponderação (amostra) e setores censitários
relacao_areap_setor <- read_tsv(
  file.path(dir_dados, 'Composicao das Areas de Ponderacao.txt'),
  locale = locale(encoding = 'UTF-16')
) %>% #filtra apenas município de São Paulo
  filter(str_sub(Setor, 1, 7) == '3550308') 
```

## Tratamento dos dados

Para poder aplicar a função do IPF, é necessário que as categorias das
duas bases de dados tenham a mesma nomenclatura. Portanto, vamos
primeiro criar as classes para os dados da amostra, a partir da variável
`V6532` (renda per capita do domicílio) com base nas categorias dos
dados do universo, a saber:

- V014: domicílios no setor sem rendimento
- V005: domicílios no setor com renda per capita de até 1/8 SM
- V006: domicílios no setor com renda per capita de 1/8 SM até 1/4 SM
- V007: domicílios no setor com renda per capita de 1/4 SM até 1/2 SM
- V008: domicílios no setor com renda per capita de 1/2 SM até 1 SM
- V009: domicílios no setor com renda per capita de 1 SM até 2 SM
- V010: domicílios no setor com renda per capita de 2 SM até 3 SM
- V011: domicílios no setor com renda per capita de 3 SM até 5 SM
- V012: domicílios no setor com renda per capita de 5 SM até 10 SM
- V013: domicílios no setor com renda per capita acima de 10 SM

Também é gerada a variável alvo com a classificação do domicílio a
partir da renda familiar, conforme já explicitado acima.

``` r
# categorias existentes 
var_restritivas <- c('V014', paste0('V', str_pad(5:13, 3, pad = '0')))

df_amostra_sp <- df_amostra %>% 
  filter(
    # filtra município de São Paulo
    paste0(V0001,V0002) == '3550308',
    # filtra domicílios que não responderam a renda
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
```

Para os dados do universo, será apenas necessário selecionar as
variáveis de interesse e também identificar os setores censitários.

``` r
df_universo_rest <- df_universo %>% 
  select(
    all_of(c(
      'Cod_setor',
      var_restritivas
    ))) %>% 
  column_to_rownames('Cod_setor') %>% 
  mutate(Cod_setor = rownames(.)) %>% 
  drop_na()
```

## Aplicação do IPF

A função abaixo foi retirada do [livro de Lovelace
(2016)](https://spatial-microsim-book.robinlovelace.net/smsimr.html#mipfp)
para posterior processamento dos pesos calculados a partir da função
`mipfp::Ipfp`. Considerando que os pesos não necessariamente são números
inteiros e que não é possível em um setor censitário conter metade de um
domicílio, é necessário que os pesos sejam transformados em números
inteiros. Para tanto, foi utilizado o método *Truncate, Replicate and
Sample*.

``` r
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
```

O IPF foi aplicado para cada área de ponderação (AP) dentro do município
de São Paulo. O processo para cada AP também já considera a inteirização
e retorna a quantidade sintética de domicílios em cada setor censitário
para cada grupo de renda familiar.

``` r
# identifica todas as aps de São Paulo
areas_ponderacao <- unique(relacao_areap_setor$`Área de ponderação`)

# rotina para cada área de ponderação
setor_grupos <- map_df(areas_ponderacao, function(ap){
  
  # seleciona os setores de uma AP no universo
  setores <- relacao_areap_setor %>% 
    filter(`Área de ponderação` == ap) %>% 
    .$Setor
  
  universo_ap <- df_universo_rest %>% 
    filter(Cod_setor %in% setores) %>% 
    select(-Cod_setor)
  
  # corrige os setores para adequar à mesma quantidade do universo
  setores <- setores[setores %in% rownames(universo_ap)]
  
  # seleciona os indivíduos da AP
  amostra_ap <- df_amostra_sp %>% 
    filter(V0011 == ap) 
  
  ## estrutura os dados de entrada 
  weight_init <- table(amostra_ap[,c('v_restritiva', 'v_alvo')])
  weight_zones <- rep(weight_init,
                      each = length(setores))
  weight_zones <- array(weight_zones,
                        dim = c(length(setores), length(var_restritivas), 3),
                        dimnames = c(list(setores), as.list(dimnames(weight_init))))
  
  # alvo de valores totais de cada setor
  target <- list(as.matrix(universo_ap)) # remove Cod_setor
  
  # ordem das variáveis restritivas
  descript <- list(1:2)
  
  ## implementação do Ipf 
  weight_mipfp <- Ipfp(seed = weight_zones, 
                       target.list = descript, 
                       target.data = target,
                       tol = 1e-5)
  weight_mipfp <- weight_mipfp$x.hat
  
  ## inteirização por Truncate, Replicate, Sample para cada setor
  walk(setores, function(setor){
    weight_mipfp[as.character(setor),,] <- int_trs(weight_mipfp[as.character(setor),,])
  })
  
  ## expansão e total da variável alvo por setor 
  # margens do universo
  margens_universo <- data.frame(Cod_setor = rownames(universo_ap),
                                 total_uni = rowSums(universo_ap))
  
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
    ) %>% 
    ungroup() %>% 
    mutate(total_sim = G1+G2+G3) %>% 
    left_join(margens_universo, by = 'Cod_setor') %>% 
    mutate(erro = total_uni - total_sim)
  
  setores_mipfp
})
```

## Comparando com a microssimulação do mestrado

``` r
# total de domicílios do resultado
sum(setor_grupos[,c('G1', 'G2', 'G3')])
```

    [1] 3574721

``` r
# total de democílios do mestrado
setor_grupos_mestrado <- read_csv(file.path(dir_proj, 'data', 'population', 'population_micro_censustract.csv'))
sum(setor_grupos_mestrado[,c('G1', 'G2', 'G3')], na.rm= T)
```

    [1] 3574349

## Próximos passos

Seria interessante incluir outras variáveis restritivas, como número de
moradores, tipo de espécie de domicílio, e condição de ocupação.
