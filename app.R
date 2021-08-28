## TODO:
## Historical classification instead rest factor
## Texts in Spanish
## Review performance
## Consider comment with Kiko in September
## Load only next date
## ERROR: Error: Can't rename columns that don't exist.
## x Column `Var1` doesn't exist. quiniela_distribution

# 1. Load sources ----
library(shiny)
library(shinydashboard)
library(shinyjs)
library(DT)
library(dplyr)
library(ggplot2)
library(lubridate)
##library(tidyquant)
library(modeest)

x <- "fa\xE7ile"
Encoding(x)
Encoding(x) <- "latin1"

source('Quiniela.R')
source('Algorithm.R')
source('laligafantasy.R',encoding = "latin1", local = TRUE)

#setwd("C:/Users/srevuelta/OneDrive/Documentos/BusinessIntelligence/Quiniela")
bestValues <- read_rds("dfBestValues.rds")
localBestValue <- 1
rachaBestValue <- getBestValue(bestValues,"RC")
diffBestValue <-  getBestValue(bestValues,"DIFF")
restBestValue <-  getBestValue(bestValues,"RF")
injuryBestValue <- getBestValue(bestValues,"IN")
rachaBackBestValue <-  round(getBestValue(bestValues,"RB"),digits = 0)
jornadas_date <<- as.list(read_csv("Jornada.csv"))
rest_info <<- as.data.frame(read_csv("Rest.csv"))
first_list <- as.list(jornadas_date[[4]])[1:length(jornadas_date[[4]])]
jp <- jornadas_date[[1]][[length(jornadas_date[[4]])]]
js <- jornadas_date[[2]][[length(jornadas_date[[4]])]]
players_injured <<- as.data.frame(read_csv(str_glue("./laliga/players_jornada_",jp,".csv")))

# 5. Header ----
header <- dashboardHeader(title = "Quiniela predictor",dropdownMenuOutput("alertMenu"))


# 6. Sidebar ----
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Home", tabName = "home", icon = icon("home"))
  )
)

# 7. Body ----
body <- dashboardBody(
  
    tabItems(
      tabItem(tabName = "home",
              fluidRow(
                  shinydashboard::box(selectInput("jornada_primera", "Date Quiniela", first_list), width=2)
              ),
              fluidRow(
                  h4(paste("Success")),
                  valueBoxOutput("AciertosQuiniela", width = 2)
              ),
              fluidRow(
                  h4(paste("Quiniela")),
                  shinydashboard::box(DT::dataTableOutput('quiniela_table'))
              ),
              fluidRow(
                  shinydashboard::box(sliderInput(inputId = "slider_injury", label = "Factor Lesionados", min = 0, max = 1, value=injuryBestValue,step=0.2), width=2),
                  shinydashboard::box(sliderInput(inputId = "slider_rest", label = "Factor Descanso", min = 0, max = 1, value=restBestValue, step=0.2), width=2),
                  shinydashboard::box(sliderInput(inputId = "slider_racha", label = "Factor Racha", min = 0, max = 1, value=rachaBestValue, step=0.2), width=2),
                  shinydashboard::box(sliderInput(inputId = "slider_back", label = "Partidos para racha", min = 3, max = 6, value=rachaBackBestValue, step=1), width=2),
                  shinydashboard::box(sliderInput(inputId = "slider_diff", label = "Diferencia cambio signo", min = 0, max = 1, value=diffBestValue, step=0.2), width=2)
              ),
              fluidRow(
                  h4(paste("Media aciertos")),
                  valueBoxOutput("AciertosMediaQuiniela", width = 2)
              ),
              fluidRow(
                  h4(paste("Tendencia aciertos")),
                  shinydashboard::box(plotOutput("quiniela_evolution"),width = 6)
              ),
              fluidRow(
                  h4(paste("Tendencia signos")),
                  shinydashboard::box(plotOutput("quiniela_distribution"),width = 6)
              ),
              fluidRow(
                  h4(paste("Distribucion signos")),
                  shinydashboard::box(plotOutput("quiniela_current_distribution"),width = 6)
              ),
              fluidRow(
                h4(paste("Primera division prediccion")),
                valueBoxOutput("AciertosPrimera", width = 2)
              ),
              fluidRow(
                 shinydashboard::box(DT::dataTableOutput('match_table_first'))
                ),
              fluidRow(
                  h4(paste("Segunda division prediccion")),
                  valueBoxOutput("AciertosSegunda", width = 2)
              ),
              fluidRow(
                  shinydashboard::box(DT::dataTableOutput('match_table_second'))
              ),
              fluidRow(
                h4(paste("Prediccion detallada")),
                valueBoxOutput("AciertosTotales", width = 2)
              ),
              fluidRow(
                shinydashboard::box(DT::dataTableOutput('match_table_details'))
                ),
              fluidRow(
                  h4(paste("Valores recomendados")),
                  shinydashboard::box(DT::dataTableOutput('best_values'))
              )
  ))
)

ui <- dashboardPage(header, sidebar, body)


# 8. Server ----
server <- function(input, output, session) {
  
    output$AciertosQuiniela <- renderValueBox({
        
        pos <- match(input$jornada_primera,jornadas_date[[4]])
        jp <- jornadas_date[[1]][[pos]]
        js <- jornadas_date[[2]][[pos]]

        jq <- getJornadaQuiniela(input$jornada_primera,jornadas_date)
        dfQuiniela <- getQuinielaCombinacionGanadora(jq) 
        dfQuiniela <- dfQuiniela %>%
            convertNames()
        
        if (isDefaultValues(input)) {
            dfPrimera <- dfPrimeraDefault
            dfSegunda <- dfSegundaDefault
        }
        else {
            local_weight <- 1
            racha_weight <- input$slider_racha
            diff <- input$slider_diff
            racha_back <- input$slider_back
            rest_factor <- input$slider_rest
            injury_factor <- input$slider_injury
            
            dfPrimera <- getDFPronostico("primera",jp,10,local_weight,racha_weight,diff,racha_back,rest_factor,injury_factor) 
            dfSegunda <- getDFPronostico("segunda",js,11,local_weight,racha_weight,diff,racha_back,rest_factor,injury_factor) 
        }
        df <- getAciertos(dfQuiniela,rbind(dfPrimera,dfSegunda))
        
        valueBox(
            sum(df$OK), paste("Aciertos Totales",sum(df$OK),"de 15"), icon = icon("thumbs-up", lib = "glyphicon"),
            color = "yellow"
        )
    })
      output$AciertosTotales <- renderValueBox({

          pos <- match(input$jornada_primera,jornadas_date[[4]])
          jp <- jornadas_date[[1]][[pos]]
          js <- jornadas_date[[2]][[pos]]

          local_weight <- 1
          racha_weight <- input$slider_racha
          diff <- input$slider_diff
          racha_back <- input$slider_back
          rest_factor <- input$slider_rest
          injury_factor <- input$slider_injury

          dfPrimera <- getDFPronostico("primera",jp,10,local_weight,racha_weight,diff,racha_back,rest_factor,injury_factor) 
          dfSegunda <- getDFPronostico("segunda",js,11,local_weight,racha_weight,diff,racha_back,rest_factor,injury_factor)
          dfQuiniela1 <- getDFQuiniela("primera",jp,10,TRUE) 
          dfQuiniela2 <- getDFQuiniela("segunda",js,11,TRUE) 
          
          dfQuiniela <- rbind(dfQuiniela1,dfQuiniela2)

          df <- getAciertos(dfQuiniela,rbind(dfPrimera,dfSegunda))

          valueBox(
              sum(df$OK), paste("Aciertos Totales",sum(df$OK),"de 21"), icon = icon("thumbs-up", lib = "glyphicon"),
              color = "blue"
          )
      })
      
      output$AciertosPrimera <- renderValueBox({

          pos <- match(input$jornada_primera,jornadas_date[[4]])
          jp <- jornadas_date[[1]][[pos]]
          js <- jornadas_date[[2]][[pos]]

          local_weight <- 1
          racha_weight <- input$slider_racha
          diff <- input$slider_diff
          racha_back <- input$slider_back
          rest_factor <- input$slider_rest
          injury_factor <- input$slider_injury
          
          dfPrimera <- getDFPronostico("primera",jp,10,local_weight,racha_weight,diff,racha_back,rest_factor,injury_factor)
          dfQuiniela1 <- getDFQuiniela("primera",jp,10,TRUE) 

          df <- getAciertos(dfQuiniela1,dfPrimera)

          valueBox(
              sum(df$"OK"), paste("Aciertos Primera",sum(df$"OK"),"de 10"), icon = icon("thumbs-up", lib = "glyphicon"),
              color = "blue"
          )
      })
      
      output$AciertosSegunda <- renderValueBox({
          
          pos <- match(input$jornada_primera,jornadas_date[[4]])
          jp <- jornadas_date[[1]][[pos]]
          js <- jornadas_date[[2]][[pos]]
          
          local_weight <- 1
          racha_weight <- input$slider_racha
          diff <- input$slider_diff
          racha_back <- input$slider_back
          rest_factor <- input$slider_rest
          injury_factor <- input$slider_injury
          
          dfSegunda <- getDFPronostico("segunda",js,11,local_weight,racha_weight,diff,racha_back,rest_factor,injury_factor)
          dfQuiniela2 <- getDFQuiniela("segunda",js,11,TRUE) 
          df <- getAciertos(dfQuiniela2,dfSegunda)

          valueBox(
              sum(df$OK), paste("Aciertos Segunda",sum(df$OK),"de 11"), icon = icon("thumbs-up", lib = "glyphicon"),
              color = "blue"
          )
      })
      output$quiniela_table <- DT::renderDataTable({
          
          pos <- match(input$jornada_primera,jornadas_date[[4]])
          jp <- jornadas_date[[1]][[pos]]
          js <- jornadas_date[[2]][[pos]]
          
          local_weight <- 1
          racha_weight <- input$slider_racha
          diff <- input$slider_diff
          racha_back <- input$slider_back
          rest_factor <- input$slider_rest
          injury_factor <- input$slider_injury
          
          dfPrimera <- getDFPronostico("primera",jp,10,local_weight,racha_weight,diff,racha_back,rest_factor,injury_factor)
          dfSegunda <- getDFPronostico("segunda",js,11,local_weight,racha_weight,diff,racha_back,rest_factor,injury_factor)
          dfPronosticos <- rbind(dfPrimera,dfSegunda)
          
          jq <- getJornadaQuiniela(input$jornada_primera,jornadas_date)
          dfQuiniela <- getQuinielaCombinacionGanadora(jq) 
          dfQuiniela <- dfQuiniela %>%
              convertNames()
          
          df <- getAciertosQuiniela(dfQuiniela,dfPronosticos)
            
            df %>% 
                select(Team1,Team2,Pronostico,'1X2',OK) %>% 
                rename("Real" = '1X2') %>%
                datatable(options = list(pageLength = 15)) %>% 
                formatStyle(
                    'Real', 'OK',
                    backgroundColor = styleEqual(c(0, 1), 
                                                 c("rgb(249, 122, 122)", "rgb(173, 255, 185)"))) 
               
      })
      output$quiniela_evolution <- renderPlot({
          
          pos <- match(input$jornada_primera,jornadas_date[[4]])
          jp <- jornadas_date[[1]][[pos]]
          js <- jornadas_date[[2]][[pos]]
          
          local_weight <- 1
          racha_weight <- input$slider_racha
          diff <- input$slider_diff
          racha_back <- input$slider_back
          rest_factor <- input$slider_rest
          injury_factor <- input$slider_injury
          
          dfEvol <- getdfEvol(local_weight,racha_weight,diff,racha_back,rest_factor,injury_factor)
          dfEvol %>%
              filter(Date < as.Date(now())) %>%
              ggplot(aes(Date,Aciertos)) +
              geom_col(fill="steelblue") +
              geom_label(aes(label = Aciertos)) +
              theme(axis.text.x = element_text(angle = 90, hjust = 1,size=14)) +
              geom_smooth(method = "lm", se = FALSE)
      })
      
      output$quiniela_distribution <- renderPlot({
          
          df <- data.frame(matrix(nrow = 0, ncol = 3))
          colnames(df) <- c("1","X","2")
          
          dfTotalQuiniela <- data.frame(matrix(nrow = 0, ncol = 3))
          colnames(dfTotalQuiniela) <- c("Team1","Team2","1X2")
          counter <- 0
          for (i in 1:length(jornadas_date$JPri)) {
             
              date <- as.Date(jornadas_date$Date[i],format = "%d-%m-%Y")
              if (date < as.Date(now())) {
                  counter <- counter + 1
                  jq <- jornadas_date$JQ[i]

                  dfQuiniela <- getQuinielaCombinacionGanadora(jq)
                  dfTotalQuiniela <- rbind(dfTotalQuiniela,dfQuiniela)
              }
          }
          df <- as.data.frame(table(dfTotalQuiniela$"1X2"))
          df %>% 
              rename("Signus" = Var1)  %>%
              filter(Signus == "1" | Signus == "2" | Signus == "X") %>%
              mutate(Freq = round(Freq/counter,digits = 2))  %>%
              ggplot(aes(Signus,Freq)) +
              geom_col(fill="steelblue") +
              geom_label(aes(label = Freq))
          
      })
      
      output$quiniela_current_distribution <- renderPlot({
          
          pos <- match(input$jornada_primera,jornadas_date[[4]])
          jp <- jornadas_date[[1]][[pos]]
          js <- jornadas_date[[2]][[pos]]
          
          local_weight <- 1
          racha_weight <- input$slider_racha
          diff <- input$slider_diff
          racha_back <- input$slider_back
          rest_factor <- input$slider_rest
          injury_factor <- input$slider_injury
          
          dfPrimera <- getDFPronostico("primera",jp,10,local_weight,racha_weight,diff,racha_back,rest_factor,injury_factor)
          dfSegunda <- getDFPronostico("segunda",js,11,local_weight,racha_weight,diff,racha_back,rest_factor,injury_factor)
          dfPronosticos <- rbind(dfPrimera,dfSegunda)
          
          jq <- getJornadaQuiniela(input$jornada_primera,jornadas_date)
          dfQuiniela <- getQuinielaCombinacionGanadora(jq) 
          dfQuiniela <- dfQuiniela %>%
              convertNames()
          
          df <- getAciertosQuiniela(dfQuiniela,dfPronosticos)
          
          df <- as.data.frame(table(dfQuiniela$"1X2"))
          df %>% 
              rename("Signus" = Var1)  %>%
              filter(Signus == "1" | Signus == "2" | Signus == "X") %>%
              ggplot(aes(Signus,Freq)) +
              geom_col(fill="green") +
              geom_label(aes(label = Freq))
          
      })
      output$AciertosMediaQuiniela <- renderValueBox({
          
          pos <- match(input$jornada_primera,jornadas_date[[4]])
          jp <- jornadas_date[[1]][[pos]]
          js <- jornadas_date[[2]][[pos]]
          
          local_weight <- 1
          racha_weight <- input$slider_racha
          diff <- input$slider_diff
          racha_back <- input$slider_back
          rest_factor <- input$slider_rest
          injury_factor <- input$slider_injury
          
          dfEvol <- getdfEvol(local_weight,racha_weight,diff,racha_back,rest_factor,injury_factor)
          n <- round(mean(dfEvol$Aciertos),digits = 2)
          valueBox(
              n, paste("Acierto Medio:",n,"de 15"), icon = icon("thumbs-up", lib = "glyphicon"),
              color = "blue"
          )
      })
      # Table ----
      output$match_table_first <- DT::renderDataTable({
          
          pos <- match(input$jornada_primera,jornadas_date[[4]])
          jp <- jornadas_date[[1]][[pos]]
          js <- jornadas_date[[2]][[pos]]
          
          local_weight <- 1
          racha_weight <- input$slider_racha
          diff <- input$slider_diff
          racha_back <- input$slider_back
          rest_factor <- input$slider_rest
          injury_factor <- input$slider_injury
          
          dfPrimera <- getDFPronostico("primera",jp,10,local_weight,racha_weight,diff,racha_back,rest_factor,injury_factor)
          dfQuiniela1 <- getDFQuiniela("primera",jp,10,TRUE) 
          
          df <- getAciertos(dfQuiniela1,dfPrimera)
          
          df %>% 
              datatable() %>% 
              formatStyle(
                  '1X2', 'OK',
                  backgroundColor = styleEqual(c(0, 1), c("rgb(249, 122, 122)", "rgb(173, 255, 185)"))) 
      })
      
      output$match_table_second <- DT::renderDataTable({
          
          pos <- match(input$jornada_primera,jornadas_date[[4]])
          jp <- jornadas_date[[1]][[pos]]
          js <- jornadas_date[[2]][[pos]]

          local_weight <- 1
          racha_weight <- input$slider_racha
          diff <- input$slider_diff
          racha_back <- input$slider_back
          rest_factor <- input$slider_rest
          injury_factor <- input$slider_injury
          
          dfSegunda <- getDFPronostico("segunda",js,11,local_weight,racha_weight,diff,racha_back,rest_factor,injury_factor)
          dfQuiniela2 <- getDFQuiniela("segunda",js,11,TRUE) 
          df <- getAciertos(dfQuiniela2,dfSegunda)
          
          df %>% 
              datatable(options = list(pageLength = 11)) %>% 
              formatStyle(
              '1X2', 'OK',
              backgroundColor = styleEqual(c(0, 1), c("rgb(249, 122, 122)", "rgb(173, 255, 185)"))) 
      })
  
      output$match_table_details <- DT::renderDataTable({
          
          pos <- match(input$jornada_primera,jornadas_date[[4]])
          jp <- jornadas_date[[1]][[pos]]
          js <- jornadas_date[[2]][[pos]]

          local_weight <- 1
          racha_weight <- input$slider_racha
          diff <- input$slider_diff
          racha_back <- input$slider_back
          rest_factor <- input$slider_rest
          injury_factor <- input$slider_injury
          
          getPronostico(jp,js,local_weight,racha_weight,diff,racha_back,rest_factor,injury_factor) %>%
              datatable((options = list(pageLength = 21)))
      })
      
      output$best_values <- DT::renderDataTable({
          
          getFilteredBestValueDf(bestValues) %>%
                datatable() 
      })
      
      output$Local_BestValue <- renderValueBox({

        valueBox(
            localBestValue,paste("Best Value Local/Visitante Factor"), icon = icon("thumbs-up", lib = "glyphicon"),
              color = "red"
          )
      })
      
      output$Racha_BestValue <- renderValueBox({
          valueBox(
             rachaBestValue, paste("Best Value Racha Factor"), icon = icon("thumbs-up", lib = "glyphicon"),
              color = "red"
          )
      })
      
      output$RachaBack_BestValue <- renderValueBox({
          valueBox(
              rachaBackBestValue, paste("Best Value Racha Back Factor"), icon = icon("thumbs-up", lib = "glyphicon"),
              color = "red"
          )
      })
      
      output$Diff_BestValue <- renderValueBox({

          valueBox(
              diffBestValue, paste("Best Value Diff Factor"), icon = icon("thumbs-up", lib = "glyphicon"),
              color = "red"
          )
      })
}

shinyApp(ui, server)

