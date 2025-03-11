library(zoo)

#' Calcula a soma móvel de uma série temporal com diferentes larguras de janela.
#' 
#' @param x Um vetor numérico representando a série temporal.
#' @param widths Um vetor de inteiros representando as larguras das janelas.
#' @return Uma lista contendo as somas móveis para cada largura de janela.
#' @export

multiple_roll_sum <- function(x, widths = c(3, 5)) {
  # Verifica se x é um vetor numérico
  if (!is.numeric(x)) {
    stop("O argumento 'x' deve ser um vetor numérico.")
  }
  # Verifica se widths é um vetor de inteiros positivos
  if (!is.numeric(widths) || any(widths <= 0) || any(widths %% 1 != 0)) {
    stop("O argumento 'widths' deve ser um vetor de inteiros positivos.")
  }
  
  resultados <- lapply(widths, function(w) {
    # Desloca a série temporal para excluir o valor da linha atual
    x_lagged <- lag(x, n = 1, default = NA)
    # Aplica a soma móvel sobre a série temporal deslocada
    rollapply(x_lagged, width = w, FUN = sum, align = "right", fill = NA)
  })
  
  names(resultados) <- paste("Width", widths, sep = "_")
  
  return(resultados)
}