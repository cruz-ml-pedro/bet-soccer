# Função para conectar ao banco de dados
conectar_db <- function() {
  dbConnect(RMySQL::MySQL(), 
            dbname = "football_data", 
            host = "football_db", 
            port = 3306, 
            user = "football_user", 
            password = "football_password")
  }