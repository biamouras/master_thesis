# Libraries ----
library(tidyverse)

# Reading files ----
# pastas
dir_origin <- here::here()
dir_new <- paste0(dir_origin, "/data/Nova/2003/")
dir_results <- paste0(dir_origin, "/results/")

# linhas que operavam em 2003
ref <- read_csv(paste(dir_new, 'LINT_SPA_2003.csv', sep=""))

# info das linhas auxiliares
linhas <- read_delim(paste(dir_new, "Linhas.txt", sep=""), delim="\t")
laura <- read_delim(paste(dir_new, "LAURA3.csv", sep=""), delim=";")
partidas <- read_csv(paste(dir_new, "Partidas.txt", sep=""))

# pontos artificiais
new_chainage <- read_csv(paste(dir_new, "LINT_SPA_200m.csv", sep=""))

# Working data  ----

# linhas - letreiro
aux <- linhas %>%
    mutate(COD_LINHA = Cod_linha) %>%
    dplyr::select(COD_LINHA, DfSenID, 
                  SvCarLetIda, SvCarLetVolta)

# laura - calcular velocidades
aux2 <- laura %>%
    mutate(COD_LINHA = trip_id,
           vIda_U = LINHEXTTPT/(LINHTMPSU*60),
           vVolta_U = LINHEXTTST/(LINHTMSPU*60),
           ext_ida = LINHEXTTPT,
           ext_volta = LINHEXTTST) %>%
    dplyr::select(COD_LINHA, ext_ida, ext_volta, vIda_U, vVolta_U)

# shape_id
aux3 <- new_chainage %>%
    group_by(COD_LINHA, SENTIDO) %>%
    summarise(fid = unique(org_fid))

# calcular velocidade media e maxima
mean <- ref %>%
    left_join(aux, by=c("COD_LINHA", "SENTIDO"="DfSenID")) %>%
    left_join(aux2, by = "COD_LINHA") %>%
    left_join(aux3, by = c("COD_LINHA", "SENTIDO")) %>%
    mutate(route_long_name = paste(SvCarLetIda, SvCarLetVolta, sep = " - "),
           trip_headsign = if_else(SENTIDO==1, SvCarLetIda, SvCarLetVolta),
           vel_U = if_else(SENTIDO==1, vIda_U, vVolta_U)) %>%
    dplyr::select(COD_LINHA, SENTIDO, route_long_name, trip_headsign, 
                  vel_U, fid) %>%
    filter(!is.infinite(vel_U)) %>%
    summarise(mean = mean(vel_U, na.rm = T))

# partidas
aux4 <- partidas %>%
    mutate(COD_LINHA = paste(SvLinCodigo, sprintf("%02d", TpLinID), sep="")) %>%
    filter(DfLinCodigo == "L", TpDiaCodigo=="U", 
           SvHorFaixa == 7) %>%
    dplyr::select(COD_LINHA, DfSenID) %>%
    group_by(COD_LINHA, DfSenID) %>%
    summarise(ct=n())

edital <- ref %>%
    left_join(aux, by=c("COD_LINHA", "SENTIDO"="DfSenID")) %>%
    left_join(aux2, by = "COD_LINHA") %>%
    left_join(aux3, by = c("COD_LINHA", "SENTIDO")) %>%
    left_join(aux4, by = c("COD_LINHA", "SENTIDO" = "DfSenID")) %>%
    filter(!is.na(ct)) %>%
    mutate(route_long_name = paste(SvCarLetIda, SvCarLetVolta, sep = " - "),
           trip_headsign = if_else(SENTIDO==1, SvCarLetIda, SvCarLetVolta),
           vel_U = if_else(SENTIDO==1, vIda_U, vVolta_U),
           trip_id = paste(COD_LINHA, SENTIDO, sep = " - ")) %>%
    dplyr::select(trip_id, COD_LINHA, SENTIDO, route_long_name, trip_headsign, 
                  vel_U, fid) %>%
    mutate(vel_U = if_else(is.infinite(vel_U), mean$mean, vel_U)) %>% 
    rename(Codigo = COD_LINHA)

dir.create(paste0(dir_origin, '/rdata/2003'), showWarnings = F)
save(edital, file=paste0(dir_origin, '/rdata/2003/edital.rda'))