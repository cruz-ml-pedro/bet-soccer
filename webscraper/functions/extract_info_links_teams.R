# *************************************************************************************************
# Função que coleta links das páginas de estatísticas dos times para um ano específico da Série A.*
# A URL muda conforme o ano: para 2024, a URL é diferente dos outros anos.                        *
# Dependendo do ano, são aplicados seletores CSS diferentes para extrair os links.                *
# Em seguida, adiciona-se a URL base aos links coletados e retorna-se a lista de URLs completas.  *
# *************************************************************************************************

coletar_links <- function(ano) {
  url <- paste0(url_base, "/", ifelse(ano == 2024, "", paste0(ano, "/")), ano, "-Serie-A-estatisticas")
  pagina <- read_html(url)
  
  # Verifica se o ano é 2024 e aplica o seletor correto
  if (ano != 2024) {
    # Para os anos != de 2024
    links <- pagina |> 
      html_nodes(".force_mobilize .left a") |>  # seletor CSS para os outros anos
      html_attr("href") |> 
      unique()
  } else {
    # Para 2024
    links <- pagina |> 
      html_nodes("img+ a") |>  # seletor CSS para o ano 2024
      html_attr("href") |> 
      unique()
  }
  
  # Adiciona a URL base ao link, se necessário
  base_url <- "https://fbref.com"
  links_completos <- paste0(base_url, links)
  
  return(links_completos)
}
# **************************************************************************************************
# Função que extrai o ID de um link. Se o link contém o ano (indicado por quatro dígitos),         *
# aplica um padrão de extração específico. Caso contrário, aplica outro padrão para URLs sem o ano.* 
# Retorna o ID extraído do link.                                                                   *
# **************************************************************************************************
extract_id <- function(link) {
  if (grepl("/[0-9]{4}/", link)) {
    # Extrai o ID para links que contêm o ano no formato de 4 dígitos
    stringr::str_match(link, ".*/(.*)/[0-9]{4}/.*")[,2]
  } else {
    # Extrai o ID para links sem o ano
    stringr::str_match(link, ".*/(.*)/.*")[,2]
  }
}

# ***************************************************************************************************
# Função que extrai o nome da equipe a partir de um link. Primeiro, captura o nome da equipe da URL.*
# Em seguida, remove acentos e substitui os hífens por espaços. Retorna o nome formatado da equipe. *
# ***************************************************************************************************

extract_team_name <- function(link) {
  # Extrai o nome da equipe da URL
  team_name <- stringr::str_match(link, ".*/([^/]+)-Estatisticas$")[,2]
  # Remove acentos do nome da equipe
  team_name <- stringi::stri_trans_general(team_name, "Latin-ASCII")
  # Substitui hífens por espaços
  nome <- stringr::str_replace_all(team_name, "-", " ")
  
  return(nome)
}