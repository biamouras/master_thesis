Criação do GTFS de 2010 a partir da OSO
================

Replicação da geração do GTFS a partir de Ordens de Serviço Operacional
feita para a
[dissertação](https://doi.org/10.11606/D.3.2020.tde-31032021-143712).

O objetivo principal foi analisar o potencial das ZEIS sobre as
desigualdades de acesso ao trabalho, comparando com o programa Minha
Casa Minha Vida (PMCMV) e a produção habitacional do mercado formal em
São Paulo. Para tanto, foi necessário identificar quantas famílias deste
município que se enquadrariam nas faixas do PMCMV, a partir da renda
familiar mensal, e o seu local de residência.

O componente de transporte para o cálculo de acessibilidade utilizou as
informações do sistema de transporte público do município de São Paulo
disponibilizados pela SPTrans. A partir de 2012 os dados foram
organizados segundo a Especificação Geral de Feeds de Transporte Público
(GTFS) estática. A GTFS estática é um conjunto de arquivos em formato
texto que contém informações de operação esperadas para o transporte
público (Google, 2019). O uso do GTFS permite o cálculo de tempos de
viagem entre pares de origem e destino usando o *Open Trip Planner*
(OTP). No entanto, para manter a continuidade temporal dos dados de
emprego utilizados (2010, 2014 e 2017), foi necessária a organização das
informações de Ordens de Serviço Operacional e dos itinerários
georreferenciados de 2010 em GTFS para o posterior cálculo dos tempos de
viagem.
