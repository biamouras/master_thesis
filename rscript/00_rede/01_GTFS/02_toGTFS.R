# Libraries ----
library(tidyverse)

# Reading files ----

# pastas
dir_origin <- here::here()
dir_model <- file.path(dir_origin, "data/Modelo")

# GTFS modelo
frequencies <- read_csv(file.path(dir_model, "frequencies.txt"))
routes <- read_csv(file.path(dir_model, 'routes.txt'))
shapes <- read_csv(file.path(dir_model, 'shapes.txt'))
stop_times <- read_csv(file.path(dir_model, 'stop_times.txt'))
stops <- read_csv(file.path(dir_model, 'stops.txt'))
trips <- read_csv(file.path(dir_model, 'trips.txt'))
fare_rules <- read_csv(file.path(dir_model, 'fare_rules.txt'))

year <- 2007

# arquivos do ano de referência
dir_new <- file.path(dir_origin, "data/Nova", year)
dir_results  <-  file.path(dir_origin, "results", year)

# trilhos ids que estavam operando
trilhos_ids <- read_csv(paste0(dir_new, '/trilhos_id_', year, '.csv'))$trip_id
# estacoes operantes de metro e cptm
stops_trilhos_id <- read_csv(paste0(dir_new, '/stops_trilhos_', year, '.csv'))$Stop_id

# edital - from tidyng2003
load(paste0(dir_origin, '/rdata/', year, '/edital.rda'))
trip_ids <- edital %>%
    select(trip_id, Codigo, Sentido)

# pontos artificiais
shape_ids <- edital %>%
    select(trip_id, shape_id)
load(paste0('rdata/', year, '/new_chainage.rda'))

# FREQUENCIES   ----

freq_head <- names(frequencies)

freq_new <- edital %>%
    mutate(start_time = '07:00:00',
           end_time = '07:59:00') %>%
    dplyr::select(freq_head) %>%
    filter(trip_id %in% trip_ids$trip_id)

# seleciona apenas os trilhos que operavam em 2003 
# e os headway_secs das 07:00 (25200)
freq_trilhos <- frequencies %>%
    filter(trip_id %in% trilhos_ids, start_time == 25200)

freq_newTrilhos <- rbind(freq_new, freq_trilhos) %>%
    mutate(start_time = '07:00:00',
           end_time = '07:59:00')

# exportando arquivo
write.csv(freq_newTrilhos, file.path(dir_results,"frequencies.txt"), row.names = F)

# ROUTES    ----

routes_head <- names(routes)

routes_new <- edital %>%
    mutate(route_id = Codigo,
           route_short_name = Codigo,
           agency_id = 1,
           route_type = 3,
           route_color = "",
           route_long_name = Nome)  %>%
    distinct(route_id, agency_id, route_short_name, route_long_name, 
             route_color, route_type) %>%
    dplyr::select(routes_head) %>%
    filter(route_id %in% trip_ids$Codigo)

routes_trilhos <- routes %>%
    filter(route_id %in% str_sub(trilhos_ids, start = 1, 
                                 end = str_length(trilhos_ids)-2))

routes_newTrilhos <- dplyr::bind_rows(routes_new, routes_trilhos)

# exportando arquivo
write.csv(routes_newTrilhos, file.path(dir_results,"routes.txt"), row.names = F)

# FARE RULES    ----
fare_new <- edital %>% 
    mutate(fare_id = if_else(is.na(Integracao), "Onibus",
                             if_else(Integracao=="CPTM" | Integracao=="CPTM/Term. de T",
                                     "Onibus_CPTM",
                                     if_else(str_detect(Integracao, "C"),
                                             "Onibus_Metro_CPTM",
                                             if_else(str_detect(Integracao, "Metrô"),
                                                     "Onibus_Metro", 
                                                     "Onibus")))),
           origin_id = "",
           destination_id = "",
           contains_id = "",
           route_id = Codigo,
           value = 1) %>%
    spread(fare_id, value) %>% 
    mutate(Onibus = 1,
           Onibus_CPTM = if_else(!is.na(Onibus_Metro_CPTM)|!is.na(Onibus_CPTM), 
                                 1, NA_real_),
           Onibus_Metro = if_else(!is.na(Onibus_Metro_CPTM)|!is.na(Onibus_Metro),
                                  1, NA_real_)) %>% 
    gather(fare_id, value, Onibus:Onibus_Metro_CPTM) %>% 
    filter(!is.na(value)) %>% 
    dplyr::select(fare_id, route_id, origin_id, destination_id, contains_id) %>% 
    mutate(fare_id = str_replace_all(fare_id, "_","+"))

routes_id <- routes_trilhos$route_id

fare_trilhos <- fare_rules %>% 
    filter(route_id %in% routes_id) %>% 
    mutate(fare_id = str_replace_all(fare_id, "Ô", "O"),
           fare_id = str_replace_all(fare_id, "ô", "o"),
           fare_id = str_replace_all(fare_id, " ", ""),
           origin_id = "",
           destination_id = "",
           contains_id = "")

fare_newTrilhos <- dplyr::bind_rows(fare_new, fare_trilhos) 

# exportando arquivo
write.csv(fare_newTrilhos, file.path(dir_results,"fare_rules.txt"), row.names = F)

# TRIPS ----

trips_head <- names(trips)

trips_new <- edital %>%
    mutate(route_id = Codigo,
           direction_id = Sentido-1,
           service_id = "U__",
           trip_headsign = Nome) %>%
    dplyr::select(trips_head) %>%
    filter(trip_id %in% trip_ids$trip_id)

trips_trilhos <- trips %>%
    filter(trip_id %in% trilhos_ids)

trips_newTrilhos <- dplyr::bind_rows(trips_new, trips_trilhos) 

# exportando arquivo
write.csv(trips_newTrilhos, file.path(dir_results,"trips.txt"), row.names = F)

# SHAPES    ----

shapes_head <- names(shapes)

shapes_new <- new_chainage %>%
    mutate(shape_pt_lat = Y,
           shape_pt_lon = X,
           shape_dist_traveled = distance) %>%
    filter(trip_id %in% trip_ids$trip_id) %>%
    arrange(distance) %>%
    group_by(trip_id) %>%
    mutate(shape_pt_sequence = 1:n()) %>%
    ungroup() %>%
    arrange(shape_id, distance) %>% 
    dplyr::select(shapes_head)

shapes_trilhos <- shapes %>%
    filter(shape_id %in% trips_trilhos$shape_id)

shapes_newTrilhos <- dplyr::bind_rows(shapes_new, shapes_trilhos) 

# exportando arquivo
write.csv(shapes_newTrilhos, file.path(dir_results,"shapes.txt"), row.names = F)

# STOPS ----

stops_head <- names(stops)

stops_unique <- new_chainage %>%
    mutate(chave = paste(Y, X, sep = "_")) %>%
    filter(trip_id %in% trip_ids$trip_id) %>%
    group_by(chave) %>%
    summarise(ct=n()) %>%
    ungroup()

stops_new <- stops_unique %>%
    mutate(stop_id = 20001:(20000+length(stops_unique$chave)),
           stop_name = as.character(stop_id),
           stop_desc = NA) %>%
    separate(chave, into = c("stop_lat", "stop_lon"), sep = "_") %>%
    mutate(stop_lat = as.numeric(stop_lat),
           stop_lon = as.numeric(stop_lon)) 

stops_chave <- stops_new 

stops_new <- stops_new%>%
    dplyr::select(stops_head)

stops_trilhos <- stops %>%
    filter(stop_id %in% stops_trilhos_id) 

stops_newTrilhos <- dplyr::bind_rows(stops_new, stops_trilhos) 

# exportando arquivo
write.csv(stops_newTrilhos, file.path(dir_results,"stops.txt"), row.names = F)

# STOP TIMES ----

st_new <- shapes_new %>%
    left_join(shape_ids, by="shape_id") %>%
    left_join(stops_chave, by = c("shape_pt_lon" = "stop_lon", 
                                  "shape_pt_lat" = "stop_lat")) %>%
    left_join(edital, by = "trip_id") %>%
    mutate(tempo_seg = seconds_to_period(shape_dist_traveled/vel_u),
           arrival_time = sprintf("%02g:%02g:%02g", 
                                  tempo_seg@hour+7, 
                                  minute(tempo_seg),
                                  trunc(second(tempo_seg))),
           departure_time = arrival_time,
           stop_sequence = shape_pt_sequence) %>%
    dplyr::select(trip_id, arrival_time, departure_time, stop_id, stop_sequence) %>%
    filter(arrival_time!="NaN:NaN:NaN")


st_trilhos <- stop_times %>%
    filter(stop_id %in% stops_trilhos_id) %>%
    mutate(arrival_time = seconds_to_period(arrival_time),
           arrival_time = sprintf("%02g:%02g:%02g", 
                                  if_else(arrival_time@hour>0, 
                                          arrival_time@hour+3,
                                          arrival_time@hour+7),
                                  minute(arrival_time),
                                  trunc(second(arrival_time))),
           departure_time = seconds_to_period(departure_time),
           departure_time = sprintf("%02g:%02g:%02g", 
                                    if_else(departure_time@hour>0, 
                                            departure_time@hour+3,
                                            departure_time@hour+7), 
                                    minute(departure_time),
                                    trunc(second(departure_time))))

st_newTrilhos <- bind_rows(st_new, st_trilhos)

# exportando arquivo
write.csv(st_newTrilhos, file.path(dir_results,"stop_times.txt"), row.names = F)
