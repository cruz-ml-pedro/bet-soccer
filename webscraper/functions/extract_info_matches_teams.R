# ***********************************************************************************************
# Esta função recebe uma lista de links e extrai os IDs desses links usando expressões regulares.* 
# Em seguida, cria um dataframe contendo o ID da partida e o link completo.                      *
# ************************************************************************************************
extrair_ids_df <- function(links) {
  prefixo_base <- "https://fbref.com"
  ids <- gsub(".*/partidas/([^/]+)/.*", "\\1", links)
  df <- data.frame(ID_partidas = ids, 
                   Links_completos = paste0(prefixo_base, links),
                   stringsAsFactors = FALSE)
  return(df)
  }

# ********************************************************************************
# Esta função processa os dados de uma temporada específica do campeonato.       *
# Ela começa construindo a URL da página da temporada com base no ano informado e*
# define os seletores HTML para identificar os dados de interesse.               *  
# ********************************************************************************  
processar_ano <- function(ano) {
    tryCatch({
      # Definir o URL e o seletor com base no ano
      if (ano != 2024) {
        url <- paste0("https://fbref.com/pt/comps/24/", ano, "/cronograma/", ano, "-Serie-A-Resultados-e-Calendarios")
        div_seletor <- paste0("#div_sched_", ano, "_24_1")
        link_seletor <- ".left~ .left+ .left a"
      } else {
        url <- "https://fbref.com/pt/comps/24/cronograma/Serie-A-Resultados-e-Calendarios"
        div_seletor <- "#div_sched_2024_24_1"
        link_seletor <- ".left:nth-child(13) a"
      }
      
      # Ler a página HTML
      pagina <- read_html(url)
      
      # Extrair a tabela de jogos
      tabela_jogos_temporada <- pagina |> 
        html_nodes(div_seletor) |> 
        html_table()
      
      # Verificar se a tabela foi encontrada
      if (length(tabela_jogos_temporada) > 0) {
        tabela_jogos_temporada <- tabela_jogos_temporada[[1]]
        
        # Renomear colunas duplicadas
        colnames(tabela_jogos_temporada) <- make.names(colnames(tabela_jogos_temporada), unique = TRUE)
        
        # Filtrar linhas com "Partida adiada" e "Confronto"
        tabela_jogos_temporada <- 
          tabela_jogos_temporada |> 
          filter(
            !is.na(Sem), 
            is.na(Notas) | Notas != "Partida adiada",   # Mantém se Notas for NA ou diferente de "Partida adiada"
            is.na(Relatório.da.Partida) | Relatório.da.Partida != "Confronto"  # Mantém se Relatório for NA ou diferente de "Confronto"
          )
        
        # Excluir colunas desnecessárias
        tabela_jogos_temporada <- tabela_jogos_temporada |> 
          select(-`Relatório.da.Partida`, -Notas)
      } else {
        stop("Tabela de jogos não encontrada.")
      }
      
      # Extrair links de jogos
      links_jogos <- pagina |> 
        html_nodes(link_seletor) |>  
        html_attr("href")
      
      # Remover links indesejados
      links_jogos <- links_jogos[grepl("/pt/partidas/", links_jogos)]
      
      # Criar dataframe de IDs e links
      df_ids_links <- extrair_ids_df(links_jogos)
      
      # Combinar tabela de jogos com IDs e links
      df_final <- bind_cols(tabela_jogos_temporada, df_ids_links)
      
      return(df_final)
    }, error = function(e) {
      message(paste("Erro ao processar o ano", ano, ":", e$message))
      return(NULL)
    })
  }


# Função para extrair o ID e montar o dataframe
extrair_ids_df <- function(links) {
  prefixo_base <- "https://fbref.com"
  ids <- gsub(".*/partidas/([^/]+)/.*", "\\1", links)
  df <- data.frame(ID_partidas = ids, 
                   Links_completos = paste0(prefixo_base, links),
                   stringsAsFactors = FALSE)
  return(df)
}


# Função para processar cada ano 
processar_ano <- function(ano) {
  tryCatch({
    # Definir o URL e o seletor com base no ano
    if (ano != 2024) {
      url <- paste0("https://fbref.com/pt/comps/24/", ano, "/cronograma/", ano, "-Serie-A-Resultados-e-Calendarios")
      div_seletor <- paste0("#div_sched_", ano, "_24_1")
      link_seletor <- ".left~ .left+ .left a"
    } else {
      url <- "https://fbref.com/pt/comps/24/cronograma/Serie-A-Resultados-e-Calendarios"
      div_seletor <- "#div_sched_2024_24_1"
      link_seletor <- ".left:nth-child(13) a"
    }
    
    # Ler a página HTML
    pagina <- read_html(url)
    
    # Extrair a tabela de jogos
    tabela_jogos_temporada <- pagina |> 
      html_nodes(div_seletor) |> 
      html_table()
    
    # Verificar se a tabela foi encontrada
    if (length(tabela_jogos_temporada) > 0) {
      tabela_jogos_temporada <- tabela_jogos_temporada[[1]]
      
      # Renomear colunas duplicadas
      colnames(tabela_jogos_temporada) <- make.names(colnames(tabela_jogos_temporada), unique = TRUE)
      
      # Filtrar linhas com "Partida adiada" e "Confronto"
      tabela_jogos_temporada <- 
        tabela_jogos_temporada |> 
        filter(
          !is.na(Sem), 
          is.na(Notas) | Notas != "Partida adiada",   # Mantém se Notas for NA ou diferente de "Partida adiada"
          is.na(Relatório.da.Partida) | Relatório.da.Partida != "Confronto"  # Mantém se Relatório for NA ou diferente de "Confronto"
        )
      
      # Excluir colunas desnecessárias
      tabela_jogos_temporada <- tabela_jogos_temporada |> 
        select(-`Relatório.da.Partida`, -Notas)
    } else {
      stop("Tabela de jogos não encontrada.")
    }
    
    # Extrair links de jogos
    links_jogos <- pagina |> 
      html_nodes(link_seletor) |>  
      html_attr("href")
    
    # Remover links indesejados
    links_jogos <- links_jogos[grepl("/pt/partidas/", links_jogos)]
    
    # Criar dataframe de IDs e links
    df_ids_links <- extrair_ids_df(links_jogos)
    
    # Combinar tabela de jogos com IDs e links
    df_final <- bind_cols(tabela_jogos_temporada, df_ids_links)
    
    return(df_final)
  }, error = function(e) {
    message(paste("Erro ao processar o ano", ano, ":", e$message))
    return(NULL)
  })
}
