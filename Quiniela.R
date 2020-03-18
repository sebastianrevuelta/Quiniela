library(rvest)
library(stringr)
library(dplyr)
library(purrr)
library(readr)


#setwd("C:/Users/srevuelta/OneDrive/Documentos/BusinessIntelligence/Quiniela")

getdfEvol <- function(local_weight,racha_weight,diff,racha_back,rest_factor) {
    evolutionNames <- c("Date","Aciertos")
    dfEvol <- data.frame(matrix(ncol = length(evolutionNames)))
    colnames(dfEvol) <- evolutionNames
    for (i in 1:length(jornadas_date$Date)) {
        date <- jornadas_date$Date[i]
        if (as.Date(date,format = "%d-%m-%Y") < today()) {
          dfEvol[i,]$Date <- date
          dfEvol[i,]$Aciertos <- fillAciertos(date,local_weight,
                                            racha_weight,diff,racha_back,rest_factor)
        }
    }
    dfEvol$Date <- as.Date(dfEvol$Date,format = "%d-%m-%Y")
    dfEvol
}

isDefaultValues <- function(input) {
    FALSE ##TODO...
    
}
fillAciertos <- function(selected_date,local_weight,racha_weight,diff,racha_back,rest_factor) {
    
    pos <- match(as.character(selected_date),jornadas_date[[4]])
    jp <- jornadas_date[[1]][[pos]]
    js <- jornadas_date[[2]][[pos]]

    dfPrimera <- getDFPronostico("primera",jp,10,local_weight,racha_weight,diff,racha_back,rest_factor) 
    dfSegunda <- getDFPronostico("segunda",js,11,local_weight,racha_weight,diff,racha_back,rest_factor) 
    dfPronosticos <- rbind(dfPrimera,dfSegunda)
    
    jq <- getJornadaQuiniela(selected_date,jornadas_date)
    dfQuiniela <- getQuiniela(jq) 
    dfQuiniela <- dfQuiniela %>%
        convertNames()
    
    df <- getAciertosQuiniela(dfQuiniela,dfPronosticos)
    sum(df$OK)

}
getPronostico <- function(jp,js,local_coef,racha_coef,diff,racha_back,rest_factor) {
    
    a <- getResults("primera",jp,10,local_coef,racha_coef,diff,racha_back,rest_factor)
    b <- getResults("segunda",js,11,local_coef,racha_coef,diff,racha_back,rest_factor)
    
    rbind(a,b)
    
}

getBestValue <- function(df,var) {
    
    df2 <- getFilteredBestValueDf(df)
    df3 <- df2 %>%
      arrange(-ATot)
    
    df3[1,][[var]]
    
}


getFilteredBestValueDf <- function(df) {
    df <- df %>%
        group_by(DIFF,RC,RB,RF) %>%
        summarise(ATot = sum(AT)) %>%
        ungroup()
    df
}

generateBestValues <- function(){
    
    dfBestValues <- data.frame(matrix(nrow = 0, ncol = 7))
    colnames(dfBestValues) <- c("JPri","JSeg","AT","DIFF","RC","RB","RF")
    counter <- 0
    jornadas_date <- as.list(read_csv("Jornada.csv"))
    for (i in 1:length(jornadas_date$JPri)) {
        
        date <- as.Date(jornadas_date$Date[i],format = "%d-%m-%Y")
        if (date < as.Date(now())) {
            jp <- jornadas_date$JPri[i]
            js <- jornadas_date$JSeg[i]
            jq <- jornadas_date$JQ[i]
            dfQuiniela <- getQuiniela(jq) 
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
                                counter <- counter + 1
                                rest_factor <- n
                                
                                dfPrimera <- getDFPronostico("primera",jp,10,local_weight,racha_weight,diff,racha_back,rest_factor) 
                                dfSegunda <- getDFPronostico("segunda",js,11,local_weight,racha_weight,diff,racha_back,rest_factor) 

                                dfPronosticos <- rbind(dfPrimera,dfSegunda)
                                
                                df <- getAciertosQuiniela(dfQuiniela,dfPronosticos)

                                dfBestValues[counter,]$JPri <- jp
                                dfBestValues[counter,]$JSeg <- js
                                dfBestValues[counter,]$AT <- sum(df$OK)
                                dfBestValues[counter,]$RC <- racha_weight
                                dfBestValues[counter,]$DIFF <- diff
                                dfBestValues[counter,]$RB <- racha_back
                                dfBestValues[counter,]$RF <- rest_factor
                            }
                        }
                    }
                }
            }
        #}
    }
    write_rds(dfBestValues,path = "dfBestValues.rds")
    
}

sliderValues <- function (inputId, label, values, from, to = NULL, width = NULL) {
    sliderProps <- shiny:::dropNulls(list(class = "js-range-slider", 
                                          id = inputId,  
                                          `data-type` = if (!is.null(to)) "double",
                                          `data-from` = which(values == from) - 1,
                                          `data-to` = if (!is.null(to)) which(values == to) - 1,
                                          `data-grid` = TRUE,
                                          `data-values` = paste(values, collapse = ", ")
    ))
    sliderProps <- lapply(sliderProps, function(x) {
        if (identical(x, TRUE)) 
            "true"
        else if (identical(x, FALSE)) 
            "false"
        else x
    })
    sliderTag <- div(class = "form-group shiny-input-container", 
                     style = if (!is.null(width)) 
                         paste0("width: ", validateCssUnit(width), ";"), 
                     if (!is.null(label)) 
                         shiny:::controlLabel(inputId, label), do.call(tags$input, 
                                                                       sliderProps))
    dep <- list(htmltools::htmlDependency("ionrangeslider", "2.0.12", c(href = "shared/ionrangeslider"), 
                                          script = "js/ion.rangeSlider.min.js",
                                          stylesheet = c("css/ion.rangeSlider.css",
                                                         "css/ion.rangeSlider.skinShiny.css")))
    htmltools::attachDependencies(sliderTag, dep)
}

