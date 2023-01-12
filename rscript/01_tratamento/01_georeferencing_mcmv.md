Georreferenciamento do MCMV
================

- <a href="#preparação-do-ambiente-e-carregando-os-dados"
  id="toc-preparação-do-ambiente-e-carregando-os-dados">Preparação do
  ambiente e carregando os dados</a>
- <a href="#tratamento-dos-dados-para-a-geocodificação"
  id="toc-tratamento-dos-dados-para-a-geocodificação">Tratamento dos dados
  para a geocodificação</a>
- <a href="#geocodificação" id="toc-geocodificação">Geocodificação</a>
- <a href="#verificação-dos-resultados"
  id="toc-verificação-dos-resultados">Verificação dos resultados</a>

Replicação do georreferenciamento feito para a
[dissertação](https://doi.org/10.11606/D.3.2020.tde-31032021-143712).

Serão utilizadas duas bases de dados para a análise da localização da
produção habitacional em São Paulo:

- a de lançamentos imobiliários residenciais da Região Metropolitana de
  São Paulo entre 1985 e 2013 sistematizados pela Empresa Brasileira de
  Patrimônio (Embraesp) e aprimorados pelo [Centro de Estudos da
  Metrópole
  (CEM)](https://centrodametropole.fflch.usp.br/pt-br/file/15626/download?token=beIaApul).
- a de contratações do Programa Minha Casa Minha Vida (PMCMV) entre 2009
  e 2018 disponibilizados via [Lei de Acesso à Informação pela
  Controladoria Geral da
  União](http://www.consultaesic.cgu.gov.br/busca/dados/Lists/Pedido/Attachments/1301410/RESPOSTA_PEDIDO_2019_59017_000222.xlsx)

Dada a necessidade das informações espaciais das duas bases para
possibilitar a comparação e avaliação da localização dos
empreendimentos, é necessário realizar a geocodificação dos dados das
contratação do PMCMV. É importante ressaltar as limitações desta base,
informadas pela CGU:

1)  alguns contratos são referentes a diferentes unidades habitacionais
    que podem estar espacialmente distribuídas e não concentradas em um
    empreendimento;
2)  os contratos estão protegidos pelo sigilo bancário, portanto apenas
    empreendimentos da Faixa 1 são obrigados a apresentar as informações
    necessárias para fazer a geocodificação;

Para tanto, será feita uma limpeza nos dados de endereço e utilizada a
API do Google para obter as coordenadas dos empreendimentos do PMCMV em
São Paulo.

## Preparação do ambiente e carregando os dados

``` r
# bibliotecas necessárias
library(tidyverse)
library(sf)
library(readxl)
library(leaflet)

# pasta do projeto
dir_proj <- here::here()
# pasta com os dados
dir_dados <- file.path(dir_proj, 'data', 'housing', 'mcmv')
```

Leitura do dado bruto.

``` r
xls <- read_xlsx(file.path(dir_dados, 'RESPOSTA_PEDIDO_2019_59017_000222.xlsx'),
                 skip = 8)

head(xls)
```

    # A tibble: 6 × 19
      Código\r\…¹ Origem UF    Munic…² Faixa Fase …³ Modal…⁴ UH\r\…⁵ UH\r\…⁶ UH\r\…⁷
      <chr>       <chr>  <chr> <chr>   <chr>   <dbl> <chr>     <dbl>   <dbl>   <dbl>
    1 120020      PF     AC    Cruzei… Faix…       1 CCFGTS        1       1       1
    2 120040      PF     AC    Rio Br… Faix…       1 CCFGTS        1       1       1
    3 120040      PF     AC    Rio Br… Faix…       1 CCFGTS        6       6       6
    4 120013      PF     AC    Bujari  Faix…       1 CCFGTS        1       1       1
    5 120020      PF     AC    Cruzei… Faix…       1 CCFGTS        2       2       2
    6 120034      PF     AC    Manoel… Faix…       1 CCFGTS        2       2       2
    # … with 9 more variables: `Valor\r\nTotal` <dbl>, IF <chr>,
    #   `Percentual\r\nde obra (%)` <dbl>, `Nome do\r\nEmpreendimento` <chr>,
    #   Endereço <chr>, Construtora <chr>, `CNPJ\r\nConstrutora` <chr>,
    #   Data <dttm>, `Ano de\r\nContratação` <chr>, and abbreviated variable names
    #   ¹​`Código\r\ndo IBGE`, ²​Município, ³​`Fase PMCMV`, ⁴​Modalidade,
    #   ⁵​`UH\r\nContratadas`, ⁶​`UH\r\nEntregues`, ⁷​`UH\r\nConcluídas`

Como os nomes das colunas têm caracteres especiais, para facilitar a
manipulação, vou renomeá-las.

``` r
# renomeia as colunas
colnames(xls) <- c("cod_ibge", "pf_pj", "uf", "municipio", "faixa", "fase_mcmv", 
                   "modalidade", "uh_contratadas", "uh_entregues", "uh_concluidas", 
                   "valor_tot", "if", "and_obra", "empreendimento", "endereco",
                   "construtora", "cnpj", "data", "ano_contratacao")
```

## Tratamento dos dados para a geocodificação

Para obter as coordenadas geográficas, preciso dos endereços dos
empreendimentos. Como vou trabalhar apenas com o município de São Paulo,
primeiro vou filtrar apenas os empreendimentos dentro da região e então
verificar se os dados estão limpos e compatíveis para a análise.

``` r
mcmv_sp <- xls %>% 
  filter(
    # filtra empreendimentos em SP
    municipio == 'São Paulo',
    # filtra projetos que não são de urbanização
    modalidade != 'FAR - Urbanização'
  )

# verifica tamanho do novo df
dim(mcmv_sp)
```

    [1] 700  19

``` r
# obtém os valores únicos de endereços
head(unique(mcmv_sp$endereco))
```

    [1] NA                             "RUA ANTONIO JULIO DOS SANTOS"
    [3] "AV CORONEL PIRES DE ANDRADE"  "AVENIDA SAO MIGUEL, 1956"    
    [5] "RUA CARUBINHA SEM NUMERO"     "RUA ITAJUIBE S/N"            

Olhando apenas os primeiros seis endereços, já é possível identificar
entradas com diferentes padrões, como “Avenida” e indicação de “Sem
número”.

``` r
# cria df com enderecos originais
enderecos <- mcmv_sp %>%
  select(
      end_org = endereco,
      condominio = empreendimento) %>%
  filter(!is.na(end_org)) %>%
  # extrai as informações contidas no campo endereco
  mutate(
    # remove virgulas e hifens
    end = str_replace_all(end_org, ',|-|_|\\.', ' '),
    # extrai lotes
    lote = str_extract(end, '(?:LOTE|LT)\\s(?:\\d{1,3}|\\w)'),
    # extrai quadras
    quadra = str_extract(end, '(?:QUADRA|QD)\\s(?:\\d{1,3}|\\w)'),
    # identifica o que não é lote ou quadra
    end = str_replace_all(end, '(?:(?:LOTE|QUADRA|QD|LT)\\s(?:\\d{1,3}|\\w)|(?:SEM NUMERO|S/N|S\\sN|\\bSN\\b))',''),
    # extrai numero, se houver
    numero = str_extract(end, '\\d{1,9}'),
    # extrai logradouro
    logradouro = str_trim(
      if_else(is.na(numero),
        end,
        str_replace(end, numero, ''))),
    # substitui av. por avenida
    logradouro = str_replace(logradouro, '^AV\\b', 'AVENIDA'),
    # substitui al por alameda
    logradouro = str_replace(logradouro, '^AL\\b', 'ALAMEDA'),
    # substitui r por rua
    logradouro = str_replace(logradouro, '^R\\b', 'RUA'),
    endereco = paste0(
      condominio, ', ', 
      logradouro, ', ', 
      if_else(is.na(numero), '', paste0(numero, ', ')),
      'SAO PAULO')
    ) %>%
    count(endereco)

enderecos_unicos <- unique(enderecos$endereco)
```

## Geocodificação

Tendo a Chave de API válida para utilizar o API do Google, vou iterar a
lista de endereços para obter suas coordenadas geográficas, para
auxiliar na busca, também incluí os nomes dos condomínios.

``` r
# itera por endereco unico
enderecos_unicos <- map_df(enderecos_unicos, function(endereco){
  # gera a url com o endereco a ser buscado
  url <- paste0('https://maps.googleapis.com/maps/api/geocode/json?address=', endereco, '&key=', chave_api) %>%
    # substitui espaços por %20
    str_replace_all(' ', '%20')

  # busca o endereço
  r <- httr::GET(url) %>%
      httr::content()

  # gera um df com os resultados
  if(r$status == 'OK'){
    df <- tibble(
      endereco = endereco,
      status = r$status,
      location_type = r$results[[1]]$geometry$location_type,
      lon = r$results[[1]]$geometry$location$lng, 
      lat = r$results[[1]]$geometry$location$lat
  )
  df
  } 
  
}, .progress = T)
```

     ■■                                 4% |  ETA:  2m

     ■■■                                7% |  ETA:  1m

     ■■■■                              10% |  ETA:  1m

     ■■■■■                             14% |  ETA:  1m

     ■■■■■■                            17% |  ETA:  1m

     ■■■■■■■                           20% |  ETA:  1m

     ■■■■■■■■                          24% |  ETA:  1m

     ■■■■■■■■■                         27% |  ETA:  1m

     ■■■■■■■■■■                        30% |  ETA:  1m

     ■■■■■■■■■■■                       34% |  ETA:  1m

     ■■■■■■■■■■■■                      37% |  ETA:  1m

     ■■■■■■■■■■■■■                     40% |  ETA:  1m

     ■■■■■■■■■■■■■■                    44% |  ETA:  1m

     ■■■■■■■■■■■■■■■                   47% |  ETA: 48s

     ■■■■■■■■■■■■■■■■                  51% |  ETA: 44s

     ■■■■■■■■■■■■■■■■■                 54% |  ETA: 41s

     ■■■■■■■■■■■■■■■■■■                57% |  ETA: 39s

     ■■■■■■■■■■■■■■■■■■■               61% |  ETA: 35s

     ■■■■■■■■■■■■■■■■■■■■              65% |  ETA: 32s

     ■■■■■■■■■■■■■■■■■■■■■             68% |  ETA: 29s

     ■■■■■■■■■■■■■■■■■■■■■■■           72% |  ETA: 25s

     ■■■■■■■■■■■■■■■■■■■■■■■■          76% |  ETA: 22s

     ■■■■■■■■■■■■■■■■■■■■■■■■■         79% |  ETA: 18s

     ■■■■■■■■■■■■■■■■■■■■■■■■■■        83% |  ETA: 15s

     ■■■■■■■■■■■■■■■■■■■■■■■■■■■       86% |  ETA: 12s

     ■■■■■■■■■■■■■■■■■■■■■■■■■■■■      90% |  ETA:  9s

     ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■     94% |  ETA:  5s

     ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■    97% |  ETA:  2s

## Verificação dos resultados

Superficialmente, vemos que 60,7% dos empreendimentos (total de 572
válidos) foram geocodificados diretamente no “telhado” (*rooftop*).

``` r
# tipos existentes
enderecos_unicos %>%
  count(location_type) %>%
  mutate(perc = n/sum(n))
```

    # A tibble: 4 × 3
      location_type          n   perc
      <chr>              <int>  <dbl>
    1 APPROXIMATE           31 0.0544
    2 GEOMETRIC_CENTER     121 0.212 
    3 RANGE_INTERPOLATED    72 0.126 
    4 ROOFTOP              346 0.607 

A partir da figura abaixo, vemos que alguns pontos aproximados estão
fora do município de SP.

``` r
enderecos_unicos %>%
  st_as_sf(coords = c('lon', 'lat'), crs = 4326) %>%
  group_by(location_type) %>%
  summarise() %>%
  ggplot() +
  ggspatial::annotation_map_tile('cartolight') +
  geom_sf(aes(color = location_type))  
```

    Loading required namespace: raster

    Zoom: 1

    Fetching 4 missing tiles


      |                                                                            
      |                                                                      |   0%
      |                                                                            
      |==================                                                    |  25%
      |                                                                            
      |===================================                                   |  50%
      |                                                                            
      |====================================================                  |  75%
      |                                                                            
      |======================================================================| 100%

    ...complete!

<img
src="01_georeferencing_mcmv_files/figure-commonmark/fig-tipos_localizacao-1.png"
id="fig-tipos_localizacao" />

Com o objetivo de refinar o processamento, vou primeiro extrair quais
são os pontos fora da área de interesse e então avaliar se é possível
obter os valores manualmente.
