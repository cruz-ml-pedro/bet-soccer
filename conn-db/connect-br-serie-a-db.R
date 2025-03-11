# Função para conectar com o DB br-serie-a-db

require("dotenv")
load_dot_env()

connect_br_serie_a_db <- function() {
  # Obtendo as credenciais do ambiente
  db_user <- Sys.getenv("DB_USER")
  db_password <- Sys.getenv("DB_PASSWORD")
  
  # Conectando ao banco de dados
  con <- dbConnect(RMySQL::MySQL(),
                   dbname = "br-serie-a-db",
                   host = "localhost",
                   port = 3309,
                   user = db_user,       
                   password = db_password)
  
  return(con)
}
