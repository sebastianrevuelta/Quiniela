## Execute
## python3 scraper.py (/home/sebas/tas/source/marca-fantasy-scraper)
## rename laliga/players.csv to /home/sebas/tas/source/quiniela/laliga/players_{jornada}.csv

## Execute
## load_quiniela_combinacion_ganadora({jornada})

## Execute generate_best_values()

## Populate Rest.csv


load_quiniela_combinacion_ganadora <- function(jornada) {
    
    url_classification <- str_glue("https://www.combinacionganadora.com/quiniela/2021-2022/jornada-",{jornada})
    html <- read_html(url_classification)
    table <- html %>%
        html_nodes(".matchTable") %>%
        html_children()
    
    dfQuiniela <- table[[2]] %>%
        html_table(fill = TRUE) %>%
        mutate("X9" = case_when(
            X4 > X6 ~ "1",
            X6 > X4 ~ "2",
            TRUE ~ "X"))
    dfQuiniela[15,]$X9 <- paste(dfQuiniela[15,]$X4,"-",dfQuiniela[15,]$X6)
    dfQuiniela[15,]$X9 <- str_replace_all(dfQuiniela[15,]$X9 , fixed(" "), "")
    
    colnames(dfQuiniela) <- c("Match","Team1","X3","X4","X5","X6","X7","Team2","1X2")
    
    dfQuiniela <- dfQuiniela %>%
        select(Team1,Team2,"1X2") 
    
    write_rds(dfQuiniela, paste0("quiniela_jornada_",jornada,".rds"))
}

generate_best_values <- function(){
    
    dfBestValues <- data.frame(matrix(nrow = 0, ncol = 8))
    colnames(dfBestValues) <- c("JPri","JSeg","AT","DIFF","RC","RB","RF","IN")
    counter <- 0
    jornadas_date <- as.list(read_csv("Jornada.csv"))
    for (i in 1:length(jornadas_date$JPri)) {
        
        date <- as.Date(jornadas_date$Date[i],format = "%d-%m-%Y")
        if (date < as.Date(now())) {
            jp <- jornadas_date$JPri[i]
            js <- jornadas_date$JSeg[i]
            jq <- jornadas_date$JQ[i]
            dfQuiniela <- getQuinielaCombinacionGanadora(jq) 
            dfQuiniela <- dfQuiniela %>%
                convertNames()
            print(paste("Calculating OK for","jornada",jp))
            #for (j in seq(0,1,by=0.1)) {
            local_weight <- 1
            for (k in seq(0,1,by=0.2)) {
                racha_weight <- k
                for (l in seq(0.2,1,by=0.2)) {
                    diff <- l
                    for (m in seq(3,6,by=1)) {
                        racha_back <- m
                        for (n in seq(0,1,by=0.2)) { 
                            rest_factor <- n
                            for (p in seq(0,1,by=0.2)) { 
                                injury_factor <- p
                                counter <- counter + 1
                            
                                dfPrimera <- getDFPronostico("primera",jp,10,local_weight,racha_weight,diff,racha_back,rest_factor,injury_factor) 
                                dfSegunda <- getDFPronostico("segunda",js,11,local_weight,racha_weight,diff,racha_back,rest_factor,injury_factor) 
                                
                                dfPronosticos <- rbind(dfPrimera,dfSegunda)
                                
                                df <- getAciertosQuiniela(dfQuiniela,dfPronosticos)
                                
                                dfBestValues[counter,]$JPri <- jp
                                dfBestValues[counter,]$JSeg <- js
                                dfBestValues[counter,]$AT <- sum(df$OK)
                                dfBestValues[counter,]$RC <- racha_weight
                                dfBestValues[counter,]$DIFF <- diff
                                dfBestValues[counter,]$RB <- racha_back
                                dfBestValues[counter,]$RF <- rest_factor
                                dfBestValues[counter,]$IN <- injury_factor
                            }
                        }
                    }
                }
            }
        }
    }
    write_rds(dfBestValues,path = "dfBestValues.rds")
    
}
