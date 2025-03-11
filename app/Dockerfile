# Use a imagem base com R e Shiny Server
FROM rocker/shiny:latest

# Defina o diretório de trabalho
WORKDIR /app

# Instalar pacotes do sistema que o app precisa
# Instalar pacotes do sistema necessários
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    default-libmysqlclient-dev \
    && rm -rf /var/lib/apt/lists/*
    
# Instalar pacotes necessários do R
RUN R -e "install.packages(c('shiny', 'httr', 'DBI', 'RMySQL', 'dplyr', 'stringr'))"

# Copiar o arquivo principal do Shiny app
COPY app.R /app/

# Copiar as pastas dos módulos e funções
COPY module_input /app/module_input
COPY module_output /app/module_output
COPY functions /app/functions

# Expor a porta padrão do Shiny Server
EXPOSE 3838

# Comando para rodar o Shiny Server
CMD ["R", "-e", "shiny::runApp('/app', host = '0.0.0.0', port = 3838)"]
