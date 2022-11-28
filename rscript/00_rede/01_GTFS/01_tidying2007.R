# Libraries ----
library(tidyverse)
library(lubridate)

# Reading files ----
# pastas
# pastas
dir_origin <- here::here()
dir_new <- paste0(dir_origin, "/data/Nova/2007/")
dir_results <- paste0(dir_origin, "/results/")

# linhas que operavam em 2007
ref <- read_csv(file.path(dir_new, 'Linhas_Empresa_util_PicoManha.csv'),
                locale=locale(encoding = "latin1"))

# pontos artificiais
new_chainage <- read_csv(file.path(dir_new, "Linhas_Empresa_util_PicoManha_200m.csv"))

# Working data  ----

edital <- ref %>% 
    mutate(headway_secs = 3600/Max_5_8_Se,
           frota_se = Frota*Max_5_8_Se/(Max_5_8_Se+Max_5_8_Re),
           t_ciclo = frota_se*headway_secs,
           vel_u = Km_Sent*1000/t_ciclo,
           shape_id = 1:n()) %>% 
    filter(Max_5_8_Se>0) 

mean_vel <- edital %>% 
    filter(!is.infinite(vel_u)) %>% 
    summarise(mean = mean(vel_u)) %>% 
    .$mean


edital <- edital %>% 
    mutate(vel_u = if_else(is.infinite(vel_u), mean_vel,vel_u)) %>% 
    dplyr::select(trip_id, Codigo, Sentido, Nome, 
                  Integracao, headway_secs, vel_u, shape_id)

save(edital, file="rdata/edital.rda")

new_chainage <-  new_chainage %>% 
    left_join(edital[,c("trip_id", "shape_id")], by="trip_id") %>% 
    mutate(Y=y) %>% 
    dplyr::select(trip_id, Codigo, Sentido, shape_id, distance, X, Y) %>% 
    arrange(shape_id, distance) %>% 
    unique(.)
save(new_chainage, file = "rdata/new_chainage.rda")
