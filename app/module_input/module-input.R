# mod_input_ui.R
mod_input_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # Substituir por um seletor para escolher a partida
    selectInput(ns("input_data"), "Escolha a partida", choices = NULL),
    actionButton(ns("submit"), "Enviar para API")
  )
}

    # mod_input_server.R
mod_input_server <- function(id, api_url, latest_matches) {
    moduleServer(id, function(input, output, session) {
        result <- reactiveVal(NULL)
    
    # Adicionar o seletor de partidas ao UI
    updateSelectInput(session, inputId = "input_data", 
                      choices = paste(latest_matches$Date, 
                                      latest_matches$Home,'VS',  
                                      latest_matches$Away))
    
    observeEvent(input$submit, {
      req(input$input_data)
      
      # Print para verificar a entrada do usuário
      #print(paste("Entrada do usuário:", input$input_data))
      # Separar a escolha do usuário no formato "Date Home vs Away"
      # Supondo que o formato é "Date Home vs Away"
      # Padrão regex para capturar a data, time 1 e time 2
      pattern <- "^(\\d{4}-\\d{2}-\\d{2})\\s+(.+)\\s+VS\\s+(.+)$"
      parts <-  str_match(input$input_data,pattern) 
      
      # Filtrar a linha correspondente no banco de dados
      match_data <- latest_matches[
        latest_matches$Date == parts[2] &
          latest_matches$Home == parts[3] &
          latest_matches$Away == parts[4], ]
      
      # Verifique se há dados correspondentes
      if (nrow(match_data) == 0) {
        showNotification("Erro: Partida não encontrada.", type = "error")
        return(NULL)
      }
      
      
      # Filtrar apenas as colunas necessárias
      match_data <-  match_data |>  select(-Date, -Home, -Away)
      
     
      # Enviar os dados para a API
      res <- httr::POST(api_url,
                        body = jsonlite::toJSON(match_data, auto_unbox = TRUE),  # Sem o "list()"
                        encode = "json",
                        httr::add_headers("Content-Type" = "application/json; charset=UTF-8"))
      
      
      if (res$status_code != 200) {
        showNotification(paste("Erro na API:", res$status_code), type = "error")
        return(NULL)
      }
      
      # Atualizar o resultado com a resposta da API
      result(httr::content(res))
    })
    
    return(result)
    })
}