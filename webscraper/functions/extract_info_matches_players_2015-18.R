# ***************************************************************************************************************************************
# Objetivo: Processar eventos de jogadores e goleiros para um time específico (casa ou visitante).                                      *
# O que faz:                                                                                                                            *          
#   Cria seletores dinâmicos para coletar informações dos jogadores e goleiros, com base no ID da equipe.                               *
# Extrai e processa dados da tabela de eventos de jogadores, gerando um resumo com informações como número de jogadores e idade mediana.*
# Extrai dados de goleiros e retorna uma lista com os resultados processados para a equipe.                                             *
# ***************************************************************************************************************************************
processar_time <- function(pagina, id_partida, equipe, equipe_id) {
  
  # Gerar seletor dinâmico para os eventos e goleiros baseado no ID da equipe
  stats_id <- paste0("#stats_", equipe_id, "_summary")
  keeper_id <- paste0("#keeper_stats_", equipe_id)
  
  # Coletar eventos dos jogadores
  eventos_jogadores <- pagina |> 
    html_nodes(stats_id) |> 
    html_table()
  
  # Verificar se a tabela de eventos foi encontrada
  if (length(eventos_jogadores) == 0) {
    warning(paste("Nenhuma tabela de eventos de jogadores encontrada para equipe:", equipe))
    return(list(eventos_jogadores = NULL, resumo_partida = NULL, eventos_goleiro = NULL))
  }
  
  # Processar a primeira tabela de eventos de jogadores
  eventos_jogadores <- eventos_jogadores[[1]] |> 
    row_to_names(row_number = 1) |> 
    mutate(ID_partida = id_partida, Equipe = equipe)
  
  # Resumo dos eventos (número de jogadores, idade mediana, ...)
  resumo_partida <- eventos_jogadores |> 
    filter(str_detect(Jogador, "\\d+ Jogadores")) |> 
    mutate(N_jogadores = str_extract(Jogador, "\\d+")) |> 
    select(-Jogador) |> 
    relocate(N_jogadores, .before = `#`)
  
  # Remover a última linha e separar a idade
  eventos_jogadores <- eventos_jogadores |> 
    filter(!str_detect(Jogador, "\\d+ Jogadores")) |> 
    separate(Idade, into = c("Anos", "Dias"), sep = "-", convert = TRUE)
  
  # Calcular a mediana da idade
  mediana_idade <- round(median(eventos_jogadores$Anos + (eventos_jogadores$Dias / 365), na.rm = TRUE), digits = 0)
  resumo_partida <- resumo_partida |> 
    mutate(Idade_mediana = mediana_idade) |> 
    select(-c("#","Nação","Idade", "Pos.", "Min.", "Gols"))
  
  # Coletar dados dos goleiros
  eventos_goleiro <- pagina |> 
    html_nodes(keeper_id) |> 
    html_table()
  
  # Verificar se a tabela de goleiros foi encontrada
  if (length(eventos_goleiro) == 0) {
    warning(paste("Nenhuma tabela de goleiros encontrada para equipe:", equipe))
    eventos_goleiro <- NULL
  } else {
    eventos_goleiro <- eventos_goleiro[[1]] |> 
      row_to_names(row_number = 1) |> 
      mutate(ID_partida = id_partida, Equipe = equipe)
  }
  
  # Retornar lista de dados processados
  list(eventos_jogadores = eventos_jogadores, resumo_partida = resumo_partida, eventos_goleiro = eventos_goleiro)
}
# *******************************************************************************************
# Objetivo: Extrair a formação tática de um time de futebol a partir de um texto.           *
# O que faz:                                                                                *
#   Usa uma expressão regular para identificar e extrair a formação tática no formato padrão* 
#   (ex: "4-4-2") de um texto que contém a formação entre parênteses.                       *
# *******************************************************************************************

extrair_formacao <- function(formacao) {
  formacao_tatica <- sub(".*\\((\\d-\\d-\\d.*)\\)", "\\1", formacao)
  return(formacao_tatica)
}
# *****************************************************************************
# Objetivo: Limpar e padronizar os nomes das equipes de futebol.              *
# O que faz:                                                                  *
#   Remove qualquer número entre parênteses, elimina textos extras como (RJ), *
#   e substitui abreviações como "Ath" por "Athletico".                       *
# Também remove acentos e espaços em excesso para padronizar o nome da equipe.*
# *****************************************************************************

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

# ******************************************************************************************
# Objetivo: Processar os valores de posse de bola para uma partida.                        *
# O que faz:                                                                               *
#   Remove o símbolo de porcentagem e converte os valores de posse de bola para numéricos. *
#  Cria um data frame com a posse de bola das equipes de casa e visitante,                 *
#  associando esses valores ao ID da partida.                                              *
# ******************************************************************************************
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

# ************************************************************************************************************
# Objetivo: Processar os dados de uma partida de futebol a partir de uma URL.                                *
# O que faz:                                                                                                 *
#   Lê a página HTML da partida e extrai informações como posse de bola, formação tática e nomes das equipes.*
# Usa a função processar_time para coletar dados de eventos e goleiros                                       *
# tanto para o time da casa quanto para o visitante.                                                         *
# Retorna uma lista de tabelas contendo formações, eventos dos jogadores e goleiros,                         *
# e posse de bola para a partida processada.                                                                 *
# Essas funções trabalham juntas para coletar e organizar as informações de partidas de futebol,             *
# obtendo dados de equipes, jogadores, goleiros e outros aspectos chave de cada jogo.                        *
# ************************************************************************************************************
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

