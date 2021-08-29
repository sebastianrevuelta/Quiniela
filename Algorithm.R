getResults <- function(category,jornada,totalTeams,local_coef,racha_coef,diff,racha_back,rest_factor,injury_factor) {
    
    ## get classification ----
    dfClassification <- getClassification(category,jornada)

    ## get jornada ----
    dfMatches <- getMatches(category,jornada,totalTeams,FALSE)

    dfNewColumns <- data.frame(matrix(nrow = totalTeams, ncol = 6))
    colnames(dfNewColumns) <- c("Team1_totalGoalsExpectedToScore","Team1_totalGoalsExpectedToReceive",
                                "Team2_totalGoalsExpectedToScore","Team2_totalGoalsExpectedToReceive",
                                "Team1_scoreRacha","Team2_scoreRacha")
                                #"Team1_injuryScore","Team2_injuryScore")
    
    dfMatchesResult <- cbind(dfMatches,dfNewColumns) 
    dfMatchesResult$Team1_totalGoalsExpectedToScore <- 0
    dfMatchesResult$Team1_totalGoalsExpectedToReceive <- 0
    dfMatchesResult$Team2_totalGoalsExpectedToScore <- 0
    dfMatchesResult$Team2_totalGoalsExpectedToReceive <- 0
    dfMatchesResult$Team1_scoreRacha <- 0
    dfMatchesResult$Team2_scoreRacha <- 0
    #dfMatchesResult$Team1_injuryScore <- 0
    #dfMatchesResult$Team2_injuryScore <- 0
    
    if (category == "primera") {
        rest_info1 <- rest_info %>% 
            filter(JPri == jornada) %>%
            rename("Team1" = "Team") %>%
            rename("PlayDuringWeek1" = "PlayDuringWeek") %>%
            select(Team1,PlayDuringWeek1)
        
        rest_info2 <- rest_info %>% 
            filter(JPri == jornada) %>%
            rename("Team2" = "Team") %>%
            rename("PlayDuringWeek2" = "PlayDuringWeek") %>%
            select(Team2,PlayDuringWeek2)
    }
    if (category == "segunda") {
        rest_info1 <- rest_info %>% 
            filter(JSeg == jornada) %>%
            rename("Team1" = "Team") %>%
            rename("PlayDuringWeek1" = "PlayDuringWeek") %>%
            select(Team1,PlayDuringWeek1)
        
        rest_info2 <- rest_info %>% 
            filter(JSeg == jornada) %>%
            rename("Team2" = "Team") %>%
            rename("PlayDuringWeek2" = "PlayDuringWeek") %>%
            select(Team2,PlayDuringWeek2)
    }
    
    dfMatchesResult <- plyr::join(dfMatchesResult,rest_info1,by=c("Team1"),match="all")
    dfMatchesResult <- plyr::join(dfMatchesResult,rest_info2,by=c("Team2"),match="all")
    if (nrow(dfMatchesResult[which(is.na(dfMatchesResult$PlayDuringWeek1)),]) > 0) {
      dfMatchesResult[which(is.na(dfMatchesResult$PlayDuringWeek1)),]$PlayDuringWeek1 <- FALSE
    }
    if (nrow(dfMatchesResult[which(is.na(dfMatchesResult$PlayDuringWeek2)),]) > 0) {
      dfMatchesResult[which(is.na(dfMatchesResult$PlayDuringWeek2)),]$PlayDuringWeek2 <- FALSE
    }
    
    summary <- dfMatchesResult %>%
        getInjuries() %>%
        getRacha(category,jornada,totalTeams,racha_back) %>%
        getPrevision(dfClassification) %>%
        mutate("GoalTeam1" = round((Team1_totalGoalsExpectedToScore+Team2_totalGoalsExpectedToReceive/2),digits=2)) %>%
        mutate("GoalTeam2" = round((Team2_totalGoalsExpectedToScore+Team1_totalGoalsExpectedToReceive/2),digits=2)) %>%
        mutate("FinalGoal1"    = round((((GoalTeam1*local_coef)+(Team1_scoreRacha*racha_coef))/(racha_coef+local_coef))-
                                         1*rest_factor*as.numeric(as.logical(PlayDuringWeek1))-
                                         injury_factor*Team1_injuryScore,digits=2)) %>%
        mutate("FinalGoal2"    = round((((GoalTeam2*local_coef)+(Team2_scoreRacha*racha_coef))/(racha_coef+local_coef))-
                                         1*rest_factor*as.numeric(as.logical(PlayDuringWeek2))-
                                         injury_factor*Team2_injuryScore,digits=2)) %>%
        mutate("FinalGoal1"  = case_when(
            FinalGoal1 < 0 ~ 0,
            TRUE ~ FinalGoal1)) %>%
        mutate("FinalGoal2"  = case_when(
            FinalGoal2 < 0 ~ 0,
            TRUE ~ FinalGoal2)) %>%
        mutate("Difference"    = FinalGoal1-FinalGoal2) %>%
        mutate("Quiniela"  = case_when(
            Difference >= diff ~ "1",
            Difference < diff & Difference > -diff ~ "X",
            TRUE ~ "2")) %>%
        select(Team1,Team2,GoalTeam1,GoalTeam2,Team1_scoreRacha,Team2_scoreRacha,Team1_injuryScore,Team2_injuryScore,FinalGoal1,FinalGoal2,Difference,Quiniela)    
    
    
    summary
}

getClassification <- function(category,jornada) {
    
    fileName <- paste0("clasificacion_",category,"_",jornada,".rds")
    if (file.exists(fileName)) {
        dfClassification <- read_rds(fileName)
    }
    else {
        url_classification <- str_glue("https://www.superdeporte.es/deportes/futbol/",{category},"-division/clasificacion-liga.html")
        html <- read_html(url_classification)
        table <- html %>%
            html_nodes(".tablaresultados") %>%
            html_children()
        
        lastChild <- length(table)
        ## The last table is the classification
        dfClassification <- table[[lastChild]] %>%
            html_table(fill = TRUE) 
        
        dfClassification <- dfClassification[-1,]
        
        colnames(dfClassification) <- c("Pos","Team","Points","PJT","PGT","PET","PPT",
                                        "GFT","GCT","PJC","PGC","PEC","PPC","GFC","GCC",
                                        "PJF","PGF","PEF","PPF","GFF","GCF")
        dfClassification$Pos <- c(1:nrow(dfClassification))
        
        cols.num <- c("Pos","Points","PJT","PGT","PET","PPT",
                      "GFT","GCT","PJC","PGC","PEC","PPC","GFC","GCC",
                      "PJF","PGF","PEF","PPF","GFF","GCF")
        dfClassification[cols.num] <- sapply(dfClassification[cols.num],as.numeric)
        
        dfClassification <- dfClassification %>%
          mutate("Team" = case_when(
            str_starts(Team,"Ala") ~ "Alaves",
            str_starts(Team,"Atl") ~ "Atletico de Madrid",
            str_ends(Team,"iz") ~ "Cadiz",
            TRUE ~ Team)) 

        write_rds(dfClassification,path = paste0("clasificacion_",category,"_",jornada,".rds"))
    }
    dfClassification
    
}

getQuinielaCombinacionGanadora <- function(jornada) {
  
  read_rds(paste0("quiniela_jornada_",jornada,".rds"))
  
}

getQuiniela <- function(jornada,category) {
    url_quiniela <- paste0("https://www.superdeporte.es/deportes/futbol/quiniela/resultados-quiniela-jornada-",jornada,".html")
    html <- read_html(url_quiniela)
    table <- html %>%
        html_nodes(".tablaresultados") %>%
        html_children()
    dfQuiniela <- table[[1]] %>%
        html_table(fill = TRUE) 

    colnames(dfQuiniela) <- c("Match","Team1","Team2","col1","colX","col2")
    
    dfQuiniela <- dfQuiniela %>%
        select(Team1,Team2,"col1","colX","col2") %>%
        mutate("1X2" = paste0(col1,colX,col2)) %>%
        select(Team1,Team2,"1X2") 
    
    dfQuiniela[15,]$"1X2" <- str_sub(dfQuiniela[15,]$"1X2",1,3)
    dfQuiniela$"1X2" <- str_remove_all(dfQuiniela$"1X2","NA")
     
    dfQuiniela
}

getMatches <- function(category,jornada,totalTeams,download) {
    
    fileName <- paste0("jornada",category,jornada,".rds")
    if (file.exists(fileName) & download == FALSE) {
        dfMatches <- read_rds(fileName)
    }
    else {
        url_matches <- str_glue("https://www.superdeporte.es/deportes/futbol/",{category},
                                "-division/calendario-liga/#",{jornada})
        html <- read_html(url_matches)
        table <- html %>%
            html_nodes(".tablaresultados") %>%
            html_children()
        dfMatches <- table[[(jornada)*2]] %>%
            html_table(fill = TRUE)
        
        colnames(dfMatches) <- c("drop1","Team1","result","Team2","drop2")
        
        dfMatches <- dfMatches %>%
            select("Team1","Team2","result") %>%
            slice(1:totalTeams) %>%
            mutate("Jornada" = jornada)
        
        dfMatches <- dfMatches %>%
            mutate("result" = case_when(
                str_trim(result) == "-" ~ "0 - 0",
                TRUE ~ result))
    
        write_rds(dfMatches,path = paste0("jornada",category,jornada,".rds"))
    }
    dfMatches
}

getRacha <- function(data,category,jornada,totalTeams,racha_back) {

    dfMatchesGlobal <- getLastMatches(category,jornada,totalTeams,racha_back)
    
    for (i in 1:nrow(data)) {
        team1 <- (data[i,]$Team1)
        score1 <- getScoreRacha(dfMatchesGlobal,team1,category,jornada,totalTeams,racha_back)
        team2 <- (data[i,]$Team2)
        score2 <- getScoreRacha(dfMatchesGlobal,team2,category,jornada,totalTeams,racha_back)
        data[i,]$"Team1_scoreRacha" <- score1
        data[i,]$"Team2_scoreRacha" <- score2
    }
    return(data)
}

getScoreRacha <- function(dfMatchesGlobal,team,category,jornada,totalTeams,racha_back) {

    matchTeam <- dfMatchesGlobal[which(dfMatchesGlobal$Team1==team | dfMatchesGlobal$Team2 == team),]
    matchTeam <- matchTeam %>%
        mutate(result =  str_replace_all(result," ","")) %>%
        mutate(goal1  = as.numeric(str_sub(result,1,1))) %>%
        mutate(goal2  = as.numeric(str_sub(result,3,3)))

    totalScore1 = sum(matchTeam[which(matchTeam$Team1==team),]$goal1)
    totalScore2 = sum(matchTeam[which(matchTeam$Team2==team),]$goal2)
    
    totalScore3 = sum(matchTeam[which(matchTeam$Team1!=team),]$goal1)
    totalScore4 = sum(matchTeam[which(matchTeam$Team2!=team),]$goal2)
    
    round(((totalScore1+totalScore2)-(totalScore3+totalScore4))/racha_back,digits=2)
}

getLastMatches <- function(category,jornada,totalTeams,racha_back) {
    
    dfMatchesGlobal <- data.frame(matrix(nrow = 0, ncol = 3))
    colnames(dfMatchesGlobal) <- c("Team1","Team2","result")

    back <- jornada-racha_back
    
    if (back > 0) {
      for (j in back:jornada-1) {
        if (j > 0) {
          dfMatches <- getMatches(category,j,totalTeams,FALSE)
          dfMatchesGlobal <- rbind(dfMatchesGlobal,dfMatches)
        }
      }
    }
    dfMatchesGlobal
}

### getPrevision Algorithm ---
getPrevision <- function(data,dfClassification) {

    for (i in 1:nrow(data)) {
        
        team1 <- (data[i,]$Team1)
        totalGoalsScoredHome <- dfClassification[which(dfClassification$Team==team1),]$GFC
        totalGoalsReceivedHome <- dfClassification[which(dfClassification$Team==team1),]$GCC
        totalMatchesHome <- dfClassification[which(dfClassification$Team==team1),]$PJC
        totalGoalsExpectedToScore <- totalGoalsScoredHome/totalMatchesHome
        totalGoalsExpectedToReceive <- totalGoalsReceivedHome/totalMatchesHome

        data[i,]$"Team1_totalGoalsExpectedToScore" <- totalGoalsExpectedToScore
        data[i,]$"Team1_totalGoalsExpectedToReceive" <- totalGoalsExpectedToReceive
        
        team2 <- (data[i,]$Team2)
        
        totalGoalsScoredOut <- dfClassification[which(dfClassification$Team==team2),]$GFF
        totalGoalsReceivedOut <- dfClassification[which(dfClassification$Team==team2),]$GCF
        totalMatchesOut <- dfClassification[which(dfClassification$Team==team2),]$PJF
        totalGoalsExpectedToScore <- totalGoalsScoredOut/totalMatchesOut
        totalGoalsExpectedToReceive <- totalGoalsReceivedOut/totalMatchesOut
        
        data[i,]$"Team2_totalGoalsExpectedToScore" <- totalGoalsExpectedToScore
        data[i,]$"Team2_totalGoalsExpectedToReceive" <- totalGoalsExpectedToReceive
    }
    data[which(is.na(data$Team1_totalGoalsExpectedToScore)),]$Team1_totalGoalsExpectedToScore <- 0
    data[which(is.na(data$Team1_totalGoalsExpectedToReceive)),]$Team1_totalGoalsExpectedToReceive <- 0
    data[which(is.na(data$Team2_totalGoalsExpectedToScore)),]$Team2_totalGoalsExpectedToScore <- 0
    data[which(is.na(data$Team2_totalGoalsExpectedToReceive)),]$Team2_totalGoalsExpectedToReceive <- 0
    return(data)
}

getAciertos <- function(dfQuiniela,dfPronostico) {
    
    dfPronostico$OK <- 0
    nAciertos <- 0
    for (i in 1:nrow(dfPronostico)) {
        team1 <- dfPronostico[i,]$Team1
        team2 <- dfPronostico[i,]$Team2
        result <- dfPronostico[i,]$"1X2"
        resultReal <- dfQuiniela[which(dfQuiniela$Team1==team1 | dfQuiniela$Team2==team2),]$"1X2"
        if (length(resultReal) > 0) {
            if (result == resultReal) {
                nAciertos <- nAciertos + 1
                dfPronostico[i,]$OK <- 1
            }
        }
    }
    
    dfPronostico
}

getAciertosQuiniela <- function(dfQuiniela,dfPronostico) {
    
    dfQuiniela$Pronostico <- "X"
    dfQuiniela$OK <- 0
    nAciertos <- 0
    
    for (i in 1:nrow(dfQuiniela)) {
        
        team1 <- dfQuiniela[i,]$Team1
        team2 <- dfQuiniela[i,]$Team2
        result <- dfQuiniela[i,]$"1X2"

        if (nrow(dfPronostico[which(dfPronostico$Team1==team1 | dfPronostico$Team2==team2),]) == 0) {
            resulPronostico <- "X"
        }
        else {
          rowPronostico <- dfPronostico[which(dfPronostico$Team1==team1 | dfPronostico$Team2==team2),]
          resulPronostico <- rowPronostico$"1X2"
          dfQuiniela[i,]$Pronostico <- resulPronostico
        }

        if (i == 15) { ## Pleno al 15
          
            gol1 <- round(as.numeric(rowPronostico$Gol1),digits = 0)
            gol2 <- round(as.numeric(rowPronostico$Gol2),digits = 0)
            if (gol1 > 2) {
                gol1 <- "M"
            }
            if (gol2 > 2) {
                gol2 <- "M"
            }
            resulPronostico <- paste0(gol1,"-",gol2)
            dfQuiniela[i,]$Pronostico <- resulPronostico
           
        }
        
        
        if (length(resulPronostico) > 0) {
            if (result == resulPronostico) {
                nAciertos <- nAciertos + 1
                dfQuiniela[i,]$OK <- 1
            }
        }
        else {
            print(paste("Not found",team1,team2))
        }
    }
    
    dfQuiniela
}
convertNames <- function(df) {
    
    df %>%
        mutate(Team1 = case_when(
                       Team1 == "Zaragoza" ~ "Real Zaragoza",
                       TRUE ~ Team1)) %>%
        mutate(Team1 = case_when(
            Team1 == "Racing" ~ "Racing de Santander",
            TRUE ~ Team1)) %>%
        mutate(Team1 = case_when(
            Team1 == "Granada" ~ "Granada CF",
            TRUE ~ Team1)) %>% 
        mutate(Team1 = case_when(
            Team1 == "Ath.Bilbao" ~ "Athletic Club",
            TRUE ~ Team1)) %>%  
        mutate(Team1 = case_when(
            Team1 == "Valladolid" ~ "Real Valladolid",
            TRUE ~ Team1)) %>%     
        mutate(Team1 = case_when(
            Team1 == "Valencia" ~ "Valencia CF",
            TRUE ~ Team1)) %>%  
        mutate(Team1 = case_when(
            Team1 == "Deportivo" ~ "Deportivo de La Coruna",
            TRUE ~ Team1)) %>%
        mutate(Team1 = case_when(
            Team1 == "Extremadura" ~ "Extremadura UD",
            TRUE ~ Team1)) %>% 
        mutate(Team1 = case_when(
            Team1 == "Sevilla FC" ~ "Sevilla",
            TRUE ~ Team1)) %>% 
        mutate(Team1 = case_when(
            Team1 == "Las Palmas" ~ "Las Palmas",
            TRUE ~ Team1)) %>% 
        mutate(Team1 = case_when(
            Team1 == "Rayo" ~ "Rayo Vallecano",
            TRUE ~ Team1)) %>% 
        mutate(Team1 = case_when(
            Team1 == "At. Madrid" ~ "Atletico de Madrid",
            TRUE ~ Team1)) %>% 
        mutate(Team1 = case_when(
          Team1 == "Sporting" ~ "Sporting de Gijon",
          TRUE ~ Team1)) %>% 
        mutate(Team1 = case_when(
          Team1 == "Cartagena" ~ "FC Cartagena",
           TRUE ~ Team1)) %>% 
       mutate(Team1 = case_when(
           Team1 == "Celta" ~ "Celta de Vigo",
           TRUE ~ Team1)) %>% 
        mutate(Team2 = case_when(
            Team2 == "Zaragoza" ~ "Real Zaragoza",
            TRUE ~ Team2)) %>%
        mutate(Team2 = case_when(
            Team2 == "Racing" ~ "Racing de Santander",
            TRUE ~ Team2)) %>%
        mutate(Team2 = case_when(
            Team2 == "Granada" ~ "Granada CF",
            TRUE ~ Team2)) %>% 
        mutate(Team2 = case_when(
            Team2 == "Ath.Bilbao" ~ "Athletic Club",
            TRUE ~ Team2)) %>%  
        mutate(Team2 = case_when(
            Team2 == "Valladolid" ~ "Real Valladolid",
            TRUE ~ Team2)) %>%     
        mutate(Team2 = case_when(
            Team2 == "Valencia" ~ "Valencia CF",
            TRUE ~ Team2)) %>%  
        mutate(Team2 = case_when(
            Team2 == "Deportivo" ~ "Deportivo de La Coruna",
            TRUE ~ Team2)) %>%
        mutate(Team2 = case_when(
            Team2 == "Extremadura" ~ "Extremadura UD",
            TRUE ~ Team2)) %>%
        mutate(Team2 = case_when(
            Team2 == "Sevilla FC" ~ "Sevilla",
            TRUE ~ Team2)) %>% 
        mutate(Team2 = case_when(
            Team2 == "Las Palmas" ~ "Las Palmas",
            TRUE ~ Team2)) %>% 
        mutate(Team2 = case_when(
            Team2 == "Rayo" ~ "Rayo Vallecano",
            TRUE ~ Team2)) %>%
        mutate(Team2 = case_when(
            Team2 == "At. Madrid" ~ "Atletico de Madrid",
            TRUE ~ Team2)) %>%     
        mutate(Team2 = case_when(
              Team2 == "Celta" ~ "Celta de Vigo",
              TRUE ~ Team2)) %>%  
        mutate(Team2 = case_when(
            Team2 == "Sporting" ~ "Sporting de Gijon",
            TRUE ~ Team2)) %>% 
        mutate(Team2 = case_when(
          Team2 == "Cartagena" ~ "FC Cartagena",
          TRUE ~ Team2)) 
    
    # [1] "Not found Granada Levante"
    # [1] "Not found Ath.Bilbao Eibar"
    # [1] "Not found Getafe Valladolid"
    # [1] "Not found Sevilla FC Villarreal"
    # [1] "Not found Valencia Real Madrid"
    # [1] "Not found Ponferradina Deportivo"
    # [1] "Not found Extremadura Málaga"
    # [1] "Not found Elche UD Las Palmas"
    # [1] "Not found Rayo Albacete"
    # [1] "Not found At. Madrid Osasuna"

}
getDFPronostico <- function(category,jornada,totalTeams,local_weight,racha_weight,diff,racha_back,rest_factor,injury_factor) {
    
    getResults(category,jornada,totalTeams,local_weight,racha_weight,diff,racha_back,rest_factor,injury_factor)  %>%
        mutate(FinalGoal1 = round(FinalGoal1,digits = 2))  %>%
        mutate(FinalGoal2 = round(FinalGoal2,digits = 2))  %>%
        select(Team1,Team2,FinalGoal1,FinalGoal2,Team1_injuryScore,Team2_injuryScore,Quiniela) %>%
        rename("Gol1" = FinalGoal1) %>%
        rename("Gol2" = FinalGoal2) %>%
        rename("1X2"  = Quiniela) 

}

getDFQuiniela <- function(category,jornada,totalTeams,download) {
    getMatches(category,jornada,totalTeams,download) %>%
        mutate(goal1  = as.numeric(str_sub(result,1,1))) %>%
        mutate(goal2  = as.numeric(str_sub(result,5,5))) %>%
        mutate("1X2"  = case_when(
            goal1 > goal2 ~ "1",
            goal2 > goal1 ~ "2",
            goal2 == goal1 ~ "X",
            TRUE ~ "?"))
}

getJornadaQuiniela <- function(date_selected,jornadas_date) {
    pos <- match(date_selected,jornadas_date[[4]])
    jq <- jornadas_date[[3]][[pos]]
    jq
}
