# ************************************************************************************************************
# A função recebe uma página HTML e processa os eventos dos jogadores e goleiros para uma equipe específica. *
# Ela utiliza seletores dinâmicos para capturar tabelas de eventos dos jogadores e estatísticas dos goleiros.*
# A função organiza e limpa os dados coletados, calcula a idade mediana dos jogadores, e                     *
# retorna uma lista contendo os eventos dos jogadores, o resumo da partida e os eventos dos goleiros.        *
# ************************************************************************************************************
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
# A função é responsável por extrair a formação tática de uma equipe a partir de uma string.*
# Ela utiliza uma expressão regular para capturar e retornar a formação.                    *
# *******************************************************************************************
extrair_formacao <- function(formacao) {
  formacao_tatica <- sub(".*\\((\\d-\\d-\\d.*)\\)", "\\1", formacao)
  return(formacao_tatica)
}

# ***********************************************************************************************
# A função realiza o pré-processamento do nome de um time, removendo informações irrelevantes,  *
# como formações numéricas e parênteses. Ela também remove acentos e padroniza nomes de equipes,*
# substituindo prefixos comuns como "Ath" por "Athletico" e "Atl" por "Atletico".               *  
# Ao final, a função retorna o nome do time limpo e padronizado.                                *
# ***********************************************************************************************

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

# ***********************************************************************************************************
# A função é responsável por processar todas as informações de uma partida de futebol a partir de uma URL.  *
# Ela extrai os nomes dos times, suas formações táticas,                                                    *
# e processa os eventos dos jogadores e goleiros para ambas as equipes (casa e visitante).                  *
# A função também organiza esses dados em tabelas e retorna uma lista com as formações e eventos da partida.*
# ***********************************************************************************************************

processar_partida <- function(url, id_equipes) {
  
  # Ler a página HTML
  pagina <- read_html(url)
  
  #extrair o ID da partida
  id_partida <- sub(".*partidas/([a-z0-9]+).*", "\\1", url)
  
  # Extrair formação das equipes
  formacoes <- pagina |> 
    html_nodes('#field_wrap tr:nth-child(1) th') |> 
    html_text()
  
  time_casa <- limpar_time(formacoes[1])
  time_visitante <- limpar_time(formacoes[2])
    
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
    eventos_goleiro_visitante = visitante$eventos_goleiro
  )
}
