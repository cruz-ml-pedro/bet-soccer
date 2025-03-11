library(shiny)
library(httr)
library(DBI)
library(RMySQL)
library(dplyr)
library(stringr)
source("module_input/module-input.R")
source("module_output/module-output.R")
source("functions/db-conn.R")

api_url <- "http://xgb-model:8000/predict"  # Substituir pelo IP correto do container


ui <- fluidPage(
    mod_input_ui("input"),
    mod_output_ui("output")
)

# Server
server <- function(input, output, session) {
  
  # Conexão com o banco de dados
    conn <- conectar_db()
  
  # Fazer uma nova query ao banco de dados para obter apenas a linha da partida selecionada
   query <- sprintf("
               SELECT Date, Home, Away, H_Lose, H_Goals_3, H_Goals_Conceded_3, H_Diff_Goals_3, H_Pts_5, H_Pts_lose_5, 
                      H_Goals_Conceded_Totals_3, H_Diff_Totals_Goals_5, H_Diff_Pts_avg, H_Goals_Conceded_avg, 
                      H_Win_avg_totals, A_Win, A_Diff_Goals_3, A_Goals_5, A_Goals_Conceded_5, A_Pts_lose_5, 
                      A_Goals_Totals_3, A_Goals_Totals_5, A_Goals_Conceded_Totals_5, A_Draw_avg, A_Draw_avg_totals, 
                      H_HTH_Win, H_HTH_Lose, H_HTH_Draw, H_HTH_Win_3, H_HTH_Draw_3, H_HTH_Win_Totals, 
                      H_HTH_Lose_Totals, H_HTH_Draw_Totals 
               FROM matches_to_model
               ORDER BY Date DESC LIMIT 11")
              
  
  # Obter as últimas 11 linhas da tabela e selecionar colunas Date, Home e Away
   latest_matches <- dbGetQuery(conn, query)
  
  # Fechar a conexão após obter os dados
    dbDisconnect(conn)
  
  # Monitorar a escolha da partida e enviar os dados para a API
  result <- mod_input_server("input", api_url, latest_matches)
  mod_output_server("output", result)
}

shinyApp(ui, server)