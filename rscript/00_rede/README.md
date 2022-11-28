# Pasta 00_rede

Contém scripts, grafos e resultados de testes e processamentos para a geração de matrizes de viagem para posterior cálculo de acessibilidade.

## Geração de GTFS (pasta 00_GTFS)

A partir de 2012 a SPTrans começou a organizar e gerar os arquivos GTFS. Para o desenvolvimento do meu mestrado, fiz alguns estudos, baseados na orientação de Renato Arbex, para a geração de GTFS a partir dos arquivos de Ordens de Serviço Operacional de 2003 e 2007. Seguindo os passos a seguir:

 1. Geração de pontos artificiais a cada 200m para toda a rede de cada ano (feitos pelo QGis);
 2. Cálculo da velocidade média esperada de cada linha de ônibus (arquivos 01_tidying2003.R e 01_tidying2007.R)
 3. Tratamento dos dados e estruturação em GTFS (apenas para 2007 - 02_2007toGTFS.R)
 
 ## Cálculo da matriz de viagens
 
 Baseado em 
'''
Pereira, R. H. M.; Grégoire, L.; Wessel, N.; Martins, J. (2019). Tutorial with reproducible example to estimate a travel time matrix
using OpenTripPlanner and Python. Retrieved from https://github.com/rafapereirabr/otp-travel-time-matrix. 
doi:10.5281/zenodo.3242134
'''

