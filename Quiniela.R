library(rvest)
library(stringr)
library(dplyr)
library(purrr)
library(readr)


#setwd("C:/Users/srevuelta/OneDrive/Documentos/BusinessIntelligence/Quiniela")

getdfEvol <- function(local_weight,racha_weight,diff,racha_back,rest_factor,injury_factor) {
    evolutionNames <- c("Date","Aciertos")
    dfEvol <- data.frame(matrix(ncol = length(evolutionNames)))
    colnames(dfEvol) <- evolutionNames
    for (i in 1:length(jornadas_date$Date)) {
        date <- jornadas_date$Date[i]
        if (as.Date(date,format = "%d-%m-%Y") < today()) {
          dfEvol[i,]$Date <- date
          dfEvol[i,]$Aciertos <- fillAciertos(date,local_weight,
                                            racha_weight,diff,racha_back,rest_factor,injury_factor)
        }
    }
    dfEvol$Date <- as.Date(dfEvol$Date,format = "%d-%m-%Y")
    dfEvol
}

isDefaultValues <- function(input) {
    FALSE ##TODO...
    
}
fillAciertos <- function(selected_date,local_weight,racha_weight,diff,racha_back,rest_factor,injury_factor) {
    
    pos <- match(as.character(selected_date),jornadas_date[[4]])
    jp <- jornadas_date[[1]][[pos]]
    js <- jornadas_date[[2]][[pos]]

    dfPrimera <- getDFPronostico("primera",jp,10,local_weight,racha_weight,diff,racha_back,rest_factor,injury_factor) 
    dfSegunda <- getDFPronostico("segunda",js,11,local_weight,racha_weight,diff,racha_back,rest_factor,injury_factor)
    dfPronosticos <- rbind(dfPrimera,dfSegunda)
    
    jq <- getJornadaQuiniela(selected_date,jornadas_date)
    dfQuiniela <- getQuinielaCombinacionGanadora(jq) 
    dfQuiniela <- dfQuiniela %>%
        convertNames()
    
    df <- getAciertosQuiniela(dfQuiniela,dfPronosticos)
    sum(df$OK)

}
getPronostico <- function(jp,js,local_coef,racha_coef,diff,racha_back,rest_factor,injury_factor) {
    
    a <- getResults("primera",jp,10,local_coef,racha_coef,diff,racha_back,rest_factor,injury_factor)
    b <- getResults("segunda",js,11,local_coef,racha_coef,diff,racha_back,rest_factor,injury_factor)
    
    rbind(a,b)
    
}

getBestValue <- function(df,var) {
    
    df2 <- getFilteredBestValueDf(df)
    df3 <- df2 %>%
      arrange(-ATot)
    
    value <- df3[1,][[var]]
    if (is.null(value)) {
      value <- 1
    }
      
    value
    
}


getFilteredBestValueDf <- function(df) {
    df <- df %>%
        group_by(DIFF,RC,RB,RF) %>%
        summarise(ATot = sum(AT)) %>%
        ungroup()
    df
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

