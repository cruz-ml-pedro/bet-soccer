# *************************************************************************************************************
# Essa função extrai a formação tática de um time a partir de um texto.                                       * 
# O parâmetro formacao contém a formação completa, e a função retorna apenas o formato numérico (ex: "4-3-3").*
# *************************************************************************************************************
extrair_formacao <- function(formacao) { 
  formacao_tatica <- sub(".*\\((\\d-\\d-\\d.*)\\)", "\\1", formacao)
  return(formacao_tatica)
}

# **************************************************************************************************
# Essa função recebe o nome de um time e faz a limpeza removendo acentos, sufixos entre parênteses,*
# espaços extras e substituindo abreviações por nomes completos, deixando o nome padronizado.      *
# **************************************************************************************************
limpar_time <- function(time) {
  # Remover qualquer formação numérica e os parênteses associados (qualquer coisa entre parênteses que tenha números)
  time_limpo <- gsub("\\s*\\(\\d.*?\\)", "", time)
  
  # Remover parênteses ao redor do conteúdo textual, como (RJ), mas mantendo o texto
  time_com_sufixo <- gsub("\\(([^0-9]+)\\)", "\\1", time_limpo)
  
  # Remover acentos
  time_sem_acentos <- iconv(time_com_sufixo, to = "ASCII//TRANSLIT")
  
  # Substituir prefixos Ath e Atl por seus respectivos nomes completos
  time_final <- gsub("\\bAth\\b", "Athletico", time_sem_acentos)
  time_final <- gsub("\\bAtl\\b", "Atletico", time_final)
  
  # Remover espaços em excesso
  time_final <- str_trim(time_final)
  
  return(time_final)
}

# *****************************************************************************************************
# Essa função processa os valores de posse de bola, removendo o símbolo de porcentagem e              *
# convertendo os dados para numéricos, criando uma tabela com as colunas Posse_casa e Posse_visitante.*
# *****************************************************************************************************
processa_posse <- function(valores, ID_partidas) {
  # Remover o símbolo de % e converter para numérico
  valores_numericos <- as.numeric(gsub("%", "", valores))
  
  # Criar um data frame com as colunas Posse_casa e Posse_visitante
  tibble(
    ID_partidas = ID_partidas,
    Posse_casa = valores_numericos[1],
    Posse_visitante = valores_numericos[2]
  )
}
# *******************************************************************************************************************************
# Essa função renomeia as colunas de um dataframe de estatísticas de goleiros, ajustando os nomes e removendo colunas genéricas.*
# Também adiciona a identificação da partida e da equipe ao dataframe.                                                          *
# *******************************************************************************************************************************
process_colunas_goleiro <- function(df, id_partida, equipe){
  # Primeira linha contém os nomes das colunas
  col_names <- df[1, ] |>  unlist()
  
  # Remove as colunas com nome genérico (``)
  valid_columns <- !str_detect(names(df), "^``$")
  
  # Renomeia as colunas apenas se o nome da coluna genérica tiver algo significativo
  new_col_names <- ifelse(valid_columns & names(df)[valid_columns] != "",
                          paste0(col_names[valid_columns], "_", names(df)[valid_columns]), 
                          col_names[valid_columns])
  
  # Garante que o vetor seja plano (não uma lista aninhada)
  new_col_names <- as.character(new_col_names)
  
  # Aplica os novos nomes às colunas
  colnames(df) <- new_col_names
  
  # Remove a primeira linha (que era os nomes antigos)
  df <- df[-1, ]
  
  df <- df |> mutate(
    ID_partida = id_partida,
    Equipe = equipe
    )
  
  return(df)
}
# ************************************************************************************************
# Essa função corrige os nomes de colunas em várias tabelas que representam eventos de jogadores,*
# aplicando as mudanças para que os nomes fiquem padronizados e consistentes.                    *  
# ************************************************************************************************
process_nomes_colunas <- function(eventos_jogadores_list) {
  
  # Função auxiliar para processar as demais tabelas
  processar_tabela <- function(df) {
    # Primeira linha contém os nomes das colunas
    col_names <- df[1, ] |>  unlist()
    
    # Remove as colunas com nome genérico (``)
    valid_columns <- !str_detect(names(df), "^``$")
    
    # Renomeia as colunas apenas se o nome da coluna genérica tiver algo significativo
    new_col_names <- ifelse(valid_columns & names(df)[valid_columns] != "",
                            paste0(col_names[valid_columns], "_", names(df)[valid_columns]), 
                            col_names[valid_columns])
    
    # Garante que o vetor seja plano (não uma lista aninhada)
    new_col_names <- as.character(new_col_names)
    
    # Aplica os novos nomes às colunas
    colnames(df) <- new_col_names
    
    # Remove a primeira linha (que era os nomes antigos)
    df <- df[-1, ]
    
    return(df)
  }
  
  # Processar a primeira tabela "summary"
  eventos_jogadores_list[["summary"]] <- eventos_jogadores_list[["summary"]] |>
    row_to_names(row_number = 1) 
  
  # Processar as demais tabelas
  for (nome_tabela in names(eventos_jogadores_list)) {
    if (nome_tabela != "summary") {
      eventos_jogadores_list[[nome_tabela]] <- processar_tabela(eventos_jogadores_list[[nome_tabela]])
    }
  }
  
  return(eventos_jogadores_list)
}

# *****************************************************************************************************************
# Essa função processa várias tabelas relacionadas a uma partida e extrai a última linha relevante de cada tabela,*
# combinando-as em um único dataframe que resume a partida.                                                       *
# *****************************************************************************************************************

process_resumo_partidas <- function(df_list, id_partida) {
  # Inicializa uma lista com o mesmo tamanho da lista original
  df_list_processada <- vector("list", length(df_list))
  
  # Função para processar cada dataframe individualmente
  processar_df <- function(df) {
    df |> 
      filter(str_detect(Jogador, "\\d+ Jogadores")) |> 
      mutate(N_jogadores = str_extract(Jogador, "\\d+")) |> 
      select(-Jogador) |> 
      relocate(N_jogadores, .before = `#`) |> 
      select(-c(`#`, Nação, Pos., Idade, Min.)) # Remove as colunas especificadas
  }
  
  # Preenche a lista final com os dataframes processados
  for (i in seq_along(df_list)) {
    df <- processar_df(df_list[[i]])
    
    # Remover colunas específicas da primeira tabela
    if (i == 1) {
      df <- df |> 
        select(-c(Gols, Assis., Cmp, `Cmp%`, Att, PrgP, Conduções, PrgC, Tent, Suc, Contatos, Crts, xAG))
    }
    
    # Remover colunas específicas da tabela número seis (misc)
    if (i == 6) {
      df <- df |> 
        select(-c(CrtsA_Desempenho, CrtV_Desempenho, Crts_Desempenho))
    }
    
    # Remover N_jogadores de todas as tabelas, exceto a primeira
    if (i != 1) {
      df <- df |>  select(-N_jogadores)
    }
    
    # Transformar em uma única linha agregando os valores
    df_list_processada[[i]] <- df |> 
      summarize(across(everything(), ~ first(.), .names = "{.col}")) # pega o primeiro valor de cada coluna
  }
  
  # Combina todas as tabelas em um único dataframe (apenas uma linha)
  df_combined <- bind_cols(df_list_processada)
  
  # Adiciona a coluna id_partida
  df_combined <- df_combined |>  mutate(ID_partida = id_partida)
  
  return(df_combined)
}
# ***********************************************************************************************************************
# Essa função processa múltiplas tabelas de eventos de jogadores de uma equipe e combina os dados em um único dataframe,*
# juntando-os por variáveis-chave e adicionando informações de idade e equipe.                                          *
# ***********************************************************************************************************************
processar_e_juntar <- function(df_list, id_partida, equipe) {
  # Função auxiliar para processar cada dataframe
  processar_df <- function(df, i) {
    # Remover a última linha
    df <- df[-nrow(df), ]
    # Remover colunas específicas da primeira tabela
    if (i == 1) {
      df <- df %>%
        select(-c(Assis., Cmp, `Cmp%`, Att, PrgP, Conduções, PrgC, Tent, Suc, Contatos, Crts, xAG))
    }
    
    # Remover colunas específicas da tabela número seis (misc)
    if (i == 6) {
      df <- df %>%
        select(-c(CrtsA_Desempenho, CrtV_Desempenho, Crts_Desempenho))
    }
    
    return(df)
  }
  
  # Itera sobre a lista e processa cada dataframe
  df_list_processada <- purrr::map2(df_list, seq_along(df_list), processar_df)
  
  # Juntar todas as tabelas usando as variáveis chave
  df_final <- purrr::reduce(df_list_processada,
                            full_join,
                            by = c("Jogador", "#", "Nação", "Pos.", "Idade", "Min.")) |> 
    separate(Idade, into = c("Anos", "Dias"), sep = "-", convert = TRUE) |> 
    mutate(
      ID_partida = id_partida,
      Equipe = equipe
    )
  
  return(df_final)
}

# *********************************************************************************
# Essa função processa as estatísticas de jogadores e goleiros de uma equipe,     *
# extraindo dados de várias seções da página e organizando as tabelas resultantes,*
# como eventos dos jogadores, resumo da partida e estatísticas de goleiros.       *
# *********************************************************************************
processar_time <- function(pagina, id_partida, equipe, equipe_id) {
  
  # Gerar seletores dinâmicos para os eventos e goleiros baseado no ID da equipe
  sufixos <- c("summary", "passing", "passing_types", "defense", "possession", "misc")
  stats_ids <- paste0("#stats_", equipe_id, "_", sufixos)
  keeper_id <- paste0("#keeper_stats_", equipe_id)
  
  # Inicializar lista para armazenar as tabelas coletadas
  eventos_jogadores_list <- list()
  
  # Coletar eventos dos jogadores para cada sufixo
  for (i in seq_along(stats_ids)) {
    tabela <- pagina |> 
      html_nodes(stats_ids[i]) |> 
      html_table()
    
    # Verificar se a tabela foi encontrada
    if (length(tabela) > 0) {
      eventos_jogadores_list[[sufixos[i]]] <- tabela[[1]] 
    } else {
      warning(paste("Nenhuma tabela de eventos de jogadores encontrada para sufixo:", sufixos[i], "equipe:", equipe))
      eventos_jogadores_list[[sufixos[i]]] <- NULL
    }
  }
  
  #Função que ajusta o nome das colunas e retorna a mesma lista
  eventos_jogadores_list  <- process_nomes_colunas(eventos_jogadores_list)
   
  #Preciso dos nomes corrigidos (função anterior) antes de extrair a última linha das tabelas e combinar o conteúdo delas
  # Função que retorna um df com o resumo das partidas (última linha das tabelas)
  resumo_partida  <- process_resumo_partidas(eventos_jogadores_list, id_partida)  
  
  # função que retorna as tabelas combinadas 
  eventos_jogadores <- processar_e_juntar(eventos_jogadores_list, id_partida, equipe)

  
  # Calcular a mediana da idade
  mediana_idade <- round(median(eventos_jogadores$Anos + (eventos_jogadores$Dias / 365), na.rm = TRUE), digits = 0)
  resumo_partida <- resumo_partida |> 
    mutate(Idade_mediana = mediana_idade) 
  
  # Coletar dados dos goleiros
    eventos_goleiro <- pagina |> 
      html_nodes(keeper_id) |> 
      html_table()
  
  # Verificar se a tabela de goleiros foi encontrada
  if (length(eventos_goleiro) == 0) {
    warning(paste("Nenhuma tabela de goleiros encontrada para equipe:", equipe))
    eventos_goleiro <- NULL
  } else {
    eventos_goleiro <- process_colunas_goleiro(eventos_goleiro[[1]], id_partida, equipe)
  }
  
  # Retornar lista de dados processados
  list(eventos_jogadores = eventos_jogadores, resumo_partida = resumo_partida, eventos_goleiro = eventos_goleiro)
}

# **********************************************************************************************************
# Essa função processa uma página HTML de uma partida de futebol, extraindo informações como posse de bola,*
# formação tática e eventos dos jogadores para as equipes da casa e visitante.                             *
# Ela organiza os dados em tabelas que podem ser armazenadas ou analisadas.                                *
# **********************************************************************************************************
processar_partida <- function(url, id_equipes) {
  
  # Ler a página HTML
  pagina <- read_html(url)
  
  #extrair o ID da partida
  id_partida <- sub(".*partidas/([a-z0-9]+).*", "\\1", url)
  
  #Extrair posse de bola
  posse_bola <- pagina |> 
    html_nodes('tr:nth-child(3) strong') |> 
    html_text()
  
  #Obter a posse de bola dos times
  posse_times <- processa_posse(posse_bola, id_partida)
  
  # Extrair formação das equipes
  formacoes <- pagina |> 
    html_nodes('#field_wrap tr:nth-child(1) th') |> 
    html_text()
  
  
  # Obter o nome dos times
  time_casa <- limpar_time(formacoes[1])
  time_visitante <- limpar_time(formacoes[2])
  
    # Obter a formação dos times
  casa_formacao <- extrair_formacao(formacoes[1])
  visitante_formacao <- extrair_formacao(formacoes[2])
  
  # Obter os IDs das equipes da tabela ID_equipes (DB)
  id_casa <- id_equipes |> filter(Equipe == time_casa) |> pull(ID_equipe)
  id_visitante <- id_equipes |> filter(Equipe == time_visitante) |> pull(ID_equipe)
  
  # Verificar se os IDs foram encontrados
  if (length(id_casa) == 0 | length(id_visitante) == 0) {
    stop("IDs das equipes não encontrados para: Casa = ", time_casa, " Visitante = ", time_visitante)
  }
  
  # Criação da tabela de formações
  formacoes_tabela <- tibble(
    ID_partida = id_partida, 
    Casa_Formacao = casa_formacao, 
    Visitante_Formacao = visitante_formacao
  )
  
  # Processar informações do time da casa
  casa <- processar_time(pagina, id_partida, time_casa, id_casa)
  
  # Processar informações do time visitante
  visitante <- processar_time(pagina, id_partida, time_visitante, id_visitante)
  
  # Retornar lista de tabelas com as informações da partida
  list(
    formacoes_tabela = formacoes_tabela,
    eventos_jogadores_casa = casa$eventos_jogadores,
    resumo_partida_casa = casa$resumo_partida,
    eventos_goleiro_casa = casa$eventos_goleiro,
    eventos_jogadores_visitante = visitante$eventos_jogadores,
    resumo_partida_visitante = visitante$resumo_partida,
    eventos_goleiro_visitante = visitante$eventos_goleiro,
    posse_bola_tabela = posse_times
  )
}

