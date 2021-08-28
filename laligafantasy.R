getInjuries <- function(data) {

    data <- data %>%
        mutate("Team1" = case_when(
            str_starts(Team1,"Ala") ~ "Alaves",
            str_starts(Team1,"Atl") ~ "Atletico de Madrid",
            str_ends("Cadiz","iz") ~ "Cadiz")) %>%
        mutate("Team2" = case_when(
            str_starts(Team2,"Ala") ~ "Alaves",
            str_starts(Team2,"Atl") ~ "Atletico de Madrid",
            str_ends("Cadiz","iz") ~ "Cadiz"))

    players_injured$counter <- 1
    players_injured <- players_injured %>%
        filter(ESTADO_JUGADOR != 'ok') %>%
        group_by(EQUIPO_ID) %>% 
        summarise("total_injured" = sum(counter)) %>%
        ungroup() %>%
        mutate("injury_coef" = total_injured/22) %>%
        mutate("Team" = case_when(
            EQUIPO_ID == "ALA" ~ "Alaves",
            EQUIPO_ID == "ATH" ~ "Athletic Club",
            EQUIPO_ID == "ATM" ~ "Atletico de Madrid",
            EQUIPO_ID == "BAR" ~ "Barcelona",
            EQUIPO_ID == "BET" ~ "Real Betis",
            EQUIPO_ID == "CAD" ~ "Cadiz",
            EQUIPO_ID == "CEL" ~ "Celta de Vigo",
            EQUIPO_ID == "ESP" ~ "Espanyol",
            EQUIPO_ID == "ELC" ~ "Elche",
            EQUIPO_ID == "GET" ~ "Getafe",
            EQUIPO_ID == "GRA" ~ "Granada CF",
            EQUIPO_ID == "LEV" ~ "Levante",
            EQUIPO_ID == "MLL" ~ "Mallorca",
            EQUIPO_ID == "OSA" ~ "Osasuna",
            EQUIPO_ID == "RAY" ~ "Rayo Vallecano",
            EQUIPO_ID == "RMA" ~ "Real Madrid",
            EQUIPO_ID == "RSO" ~ "Real Sociedad",
            EQUIPO_ID == "SEV" ~ "Sevilla",
            EQUIPO_ID == "VAL" ~ "Valencia CF",
            EQUIPO_ID == "VIL" ~ "Villarreal",
            TRUE ~ "?"))
       
    data_aux <- data %>%
        rename("Team" = "Team1")
    data_aux <- dplyr::left_join(data_aux,players_injured,by="Team")
    data_aux <- data_aux %>%
        rename("Team1_injuryScore" = "injury_coef") %>%
        rename("Team1" = "Team") %>%
        rename("Team" = "Team2") %>%
        select(-c("total_injured","EQUIPO_ID")) 
    data_aux <- dplyr::left_join(data_aux,players_injured,by="Team") 
    data_aux <- data_aux %>%
        rename("Team2_injuryScore" = "injury_coef") %>%
        rename("Team2" = "Team") %>%
        select(-c("total_injured","EQUIPO_ID"))
    
    if (nrow(data_aux[is.na(data_aux$Team1_injuryScore),]) > 0) {
      data_aux[is.na(data_aux$Team1_injuryScore),]$Team1_injuryScore <- 0
    }
    if (nrow(data_aux[is.na(data_aux$Team2_injuryScore),]) > 0) {
      data_aux[is.na(data_aux$Team2_injuryScore),]$Team2_injuryScore <- 0
    }
    
    data_aux <- data_aux %>%
       mutate("Team1_injuryScore" = round(Team1_injuryScore,digits=2)) %>%
       mutate("Team2_injuryScore" = round(Team2_injuryScore,digits=2))
    
    data_aux
}