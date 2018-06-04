library(shinydashboard)
library(googlesheets)
library(dplyr)
library(readr)
library(ggplot2)
library(plyr)
library(RJSONIO)
library(RCurl)
library(plotly)
library(googleAnalyticsR)
library(reshape2)
library(tidyverse)
#token <- gs_auth(cache = FALSE)
#gd_token()
#saveRDS(token, file = "googlesheets_token.rds")

#UI Aplikacji

header <- dashboardHeader(
  title = "Raport Finansowy Senuto"
  
)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Analiza Widoczności", tabName = "vis_explorer", icon = icon("apple", class = "fas fa-sync fa-spin"))
),
br(),
textInput(inputId = "domain_searh",
                  label = "Wpisz domenę")
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "vis_explorer",
            fluidRow(
                  h2("Obecne wartości top3/10/50"),
              valueBoxOutput("top3_last"),
              valueBoxOutput("top10_last"),
              valueBoxOutput("top50_last")
            ),
            fluidRow(
              column(12,
                     h2("Podsumowanie widoczności"),
                     br(),
                     verbatimTextOutput("Top3_summary"),
                     verbatimTextOutput("Top10_summary"),
                     verbatimTextOutput("Top50_summary"),
                     h2("Statystyki widoczności"),
                     br(),
                     dataTableOutput('explorer')
              )
            )
            
            

)
)
)


ui <- dashboardPage(
  skin = "red",
  header,
  sidebar,
  body)


#serwer

server <- function(input, output) {
  #Łączymy się z API Senuto aby pobrać statystyki widoczności
  raw_data <- getURL("http://dolphin.senuto.com/api.php?key=twoj-klucz_f122c45251f5f8818971037d310a03bf&domain=allegro.pl&type=weekly&action=domain_keywords_history")
  #Zapisujemy dane w JSon
  json_file <- fromJSON(raw_data)
  #przerabiamy dane na data set
  json_file <- lapply(json_file, function(x) {
         x[sapply(x, is.null)] <- NA
         unlist(x)
     })
  #Robimy data frame
  table_statistics <- as.data.frame(json_file, row.names = NULL)
  #Drukujemy zakładkę 
  output$explorer <- renderDataTable({
    table_statistics
  })
  
  #Drukujemy ostatnie wartości top3/10/50
  output$top3_last <- renderValueBox({
    valueBox(
      paste(last(json_file$top3)), "Obecna liczba słów w TOP3", icon = icon("users", class = "far fa-users"),
      color = "yellow")
  })
  output$top10_last <- renderValueBox({
    valueBox(
      paste(last(json_file$top10)), "Obecna liczba słów w TOP10", icon = icon("users", class = "far fa-users"),
      color = "yellow")
  })
  output$top50_last <- renderValuetbBox({
    valueBox(
      paste(last(json_file$top50)), "Obecna liczba słów w TOP50", icon = icon("users", class = "far fa-users"),
      color = "yellow")
  })
  
  #Autoryzacja konta Google Analytics
  
  ga_auth()
  
  #Pobranie listy dostępnych kont
  
  my_accounts <- ga_account_list()
  View(my_accounts)
  
  #Id profilu GA
  
  my_id <- 120888704
  
  #Ustawienia dat
  start_date <- "60daysAgo"
  end_date <- "yesterday"
  
  df2 <- google_analytics(my_id, 
                          date_range = c(start_date, end_date),
                          metrics = c("users", "sessions" ),
                          dimensions = c("date"))
  
  }

shinyApp(ui = ui, server = server)
