# ***************************************************************************************************************
# Esta função acessa uma página da web específica usando um link, ano, ID da equipe e nome da equipe fornecidos.*
# Coleta a tabela de estatísticas de jogadores, ajusta o cabeçalho e formata os dados.                          *
# Extrai os IDs dos jogadores a partir dos links na tabela e combina essas informações com a tabela principal.  *
# ***************************************************************************************************************
processar_pagina <- function(link, ano, id_equipe, equipe) {
  pagina <- read_html(link)
  
  # Coletar a tabela de interesse
  tabela <- pagina |> 
    html_nodes("#div_stats_playing_time_24") |> 
    html_table()
  
  # Garantir que a tabela foi carregada
  if (length(tabela) == 0) {
    message(paste("Tabela não encontrada em:", link))
    return(NULL)
  }
  
  # Pegar o cabeçalho e a primeira linha da tabela
  cabecalho_atual <- colnames(tabela[[1]])
  primeira_linha <- tabela[[1]][1, ]
  
  # Ajustar o cabeçalho e remover a primeira linha de dados
  colnames(tabela[[1]]) <- paste(primeira_linha, cabecalho_atual, sep = "_")
  tabela <- tabela[[1]][-1, ]
  
  # Remover a última coluna e as duas últimas linhas
  tabela <- tabela[, -ncol(tabela)]
  tabela <- tabela[-c((nrow(tabela)-1), nrow(tabela)), ]
  
  # Ajustar os nomes das colunas
  colnames(tabela) <- gsub("_$", "", colnames(tabela))
  
  # Ajustar nomes dos jogadores
  tabela$Jogador <- iconv(tabela$Jogador, from = "UTF-8", to = "ASCII//TRANSLIT")
  
  # Coletar os links dos jogadores para obter os IDs
  ids_jogadores <- pagina |> 
    html_nodes("#stats_playing_time_24 th a") |> 
    html_attr("href") 
  
  jogadores_id <- data.frame(
    ID_jogador = sub(".*/(.*)/.*", "\\1", ids_jogadores),
    Nome = gsub("-", " ", sub(".*/", "", ids_jogadores))
  )
  
  # Combinar com a tabela principal
  tabela_completa <- merge(tabela, jogadores_id, by.x = "Jogador", by.y = "Nome", all.x = TRUE)
  
  # Adicionar colunas de ano, ID_equipe e nome da equipe
  tabela_completa$Ano <- ano
  tabela_completa$ID_equipe <- id_equipe
  tabela_completa$Equipe <- equipe
  
  return(tabela_completa)
}

# ******************************************************************************************************************
# Esta função itera sobre um dataframe que contém informações sobre os links das páginas de estatísticas.          * 
# Para cada link, ela chama a função processar_pagina, lidando com possíveis erros e armazenando mensagens de erro.*
# Ao final, retorna uma lista com os resultados de todas as páginas processadas,                                   *
# permitindo a identificação de quais links falharam.                                                              *
# ******************************************************************************************************************
processar_todas_paginas <- function(df) {
  resultados <- vector("list", nrow(df))  # Alocar a lista de resultados com tamanho final
  mensagens_erro <- character(0)  # Vetor para armazenar mensagens de erro
  
  for (i in seq_len(nrow(df))) {
    Sys.sleep(10)
    link <- df$Link[i]
    ano <- df$Ano[i]
    id_equipe <- df$ID_equipe[i]
    equipe <- df$Equipe[i]
    
    resultado <- tryCatch({
      processar_pagina(link, ano, id_equipe, equipe)
    }, error = function(e) {
      # Salvar a mensagem de erro e o link correspondente
      mensagens_erro <<- c(mensagens_erro, paste("Erro ao processar link:", link, "-", e$message))
      return(NULL)  # Retornar NULL para evitar interrupção do loop
    })
    
    if (!is.null(resultado)) {
      resultados[[i]] <- resultado  # Armazenar o resultado na lista
    }
  }
  
  # Combinar todas as tabelas em um único dataframe
 # tabela_final <- do.call(rbind, resultados)
  
  # Exibir todas as mensagens de erro, se houver
  if (length(mensagens_erro) > 0) {
    message("Ocorreram erros nos seguintes links:")
    for (msg in mensagens_erro) {
      message(msg)
    }
  }
  
  return(resultados)
}

# ************************************************************************************************************
# Esta função garante que todas as tabelas em uma lista tenham as mesmas colunas.                            *    
# Ela identifica todas as colunas únicas presentes nas tabelas e                                             *
# adiciona colunas ausentes com valores NA para cada tabela.                                                 *
# Isso assegura que todas as tabelas estejam no mesmo padrão, facilitando a análise e a combinação posterior.*
# ************************************************************************************************************
harmonizar_colunas <- function(lista_tabelas) {
  todas_colunas <- unique(unlist(lapply(lista_tabelas, colnames)))  # Todas as colunas possíveis
  lista_tabelas_harmonizada <- lapply(lista_tabelas, function(tabela) {
    colunas_faltantes <- setdiff(todas_colunas, colnames(tabela))  # Identificar colunas ausentes
    for (col in colunas_faltantes) {
      tabela[[col]] <- NA  # Adicionar colunas ausentes com valores NA
    }
    return(tabela[, todas_colunas, drop = FALSE])  # Reordenar colunas para ficarem no mesmo padrão
  })
  return(lista_tabelas_harmonizada)
}

# **********************************************************************************
# Esta função verifica se certas colunas existem em uma tabela e                   *
# decide se deve removê-las ou renomeá-las com base na presença de valores NA.     * 
# Especificamente, ela compara as colunas "MP" e "MP_Tempo de jogo", garantindo que*
# apenas uma delas permaneça na tabela, dependendo dos dados presentes.            *  
# **********************************************************************************
verificar_remover_colunas <- function(tabela) {
  # Verificar se as colunas existem
  if ("MP" %in% colnames(tabela) && "MP_Tempo de jogo" %in% colnames(tabela)) {
    
    # Verifica se "MP" é apenas NA e remove se for o caso
    if (all(is.na(tabela[["MP"]]))) {
      tabela <- tabela[, !colnames(tabela) %in% "MP"]
      colnames(tabela)[colnames(tabela) == "MP_Tempo de jogo"] <- "MP_Tempo_de_jogo"
      
      # Verifica se "MP_Tempo de jogo" é apenas NA e remove se for o caso
    } else if (all(is.na(tabela[["MP_Tempo de jogo"]]))) {
      tabela <- tabela[, !colnames(tabela) %in% "MP_Tempo de jogo"]
      colnames(tabela)[colnames(tabela) == "MP"] <- "MP_Tempo_de_jogo"
      
      # Se ambas as colunas tiverem valores não-NA, renomeia "MP" para manter consistência
    } else {
      tabela <- tabela[, !colnames(tabela) %in% "MP"]
      colnames(tabela)[colnames(tabela) == "MP_Tempo de jogo"] <- "MP_Tempo_de_jogo"
    }
    
    # Se a tabela tiver apenas "MP" e não "MP_Tempo de jogo"
  } else if ("MP" %in% colnames(tabela) && !"MP_Tempo de jogo" %in% colnames(tabela)) {
    colnames(tabela)[colnames(tabela) == "MP"] <- "MP_Tempo_de_jogo"
    
    # Se a tabela tiver apenas "MP_Tempo de jogo" e não "MP"
  } else if ("MP_Tempo de jogo" %in% colnames(tabela) && !"MP" %in% colnames(tabela)) {
    colnames(tabela)[colnames(tabela) == "MP_Tempo de jogo"] <- "MP_Tempo_de_jogo"
  }
  
  return(tabela)
}

# ***********************************************************************************************************
# Esta função reordena as colunas de cada tabela em uma lista para garantir que sigam a mesma ordem.        *
# Ela identifica todas as colunas únicas e adiciona colunas faltantes com valores NA antes de reordenar.    *  
# Isso ajuda a manter uma estrutura consistente entre diferentes tabelas, facilitando análises subsequentes.*
# ***********************************************************************************************************

harmonizar_ordem_colunas <- function(lista_tabelas) {
  # Identificar todas as colunas únicas em todas as tabelas
  todas_colunas <- unique(unlist(lapply(lista_tabelas, colnames)))
  
  # Reorganizar colunas em cada tabela para que sigam a mesma ordem
  lista_tabelas_harmonizada <- lapply(lista_tabelas, function(tabela) {
    # Adicionar colunas faltantes com NA
    colunas_faltantes <- setdiff(todas_colunas, colnames(tabela))
    for (col in colunas_faltantes) {
      tabela[[col]] <- NA
    }
    
    # Reordenar as colunas de acordo com o padrão
    tabela <- tabela[, todas_colunas, drop = FALSE]
    return(tabela)
  })
  
  return(lista_tabelas_harmonizada)
}