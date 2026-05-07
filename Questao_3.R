library(plotly)
# Funcao simples para ler e contar os pares
calcula_pct <- function(arquivo) {
  # Le o arquivo e junta tudo num texto so
  linhas <- readLines(arquivo, warn = FALSE)
  dna <- paste(linhas, collapse = "")
  # Cria a matriz 4x4 zerada
  bases <- c("A", "C", "G", "T")
  matriz <- matrix(0, nrow = 4, ncol = 4)
  rownames(matriz) <- bases
  colnames(matriz) <- bases
  # Descobre o tamanho do texto
  n_letras <- nchar(dna)
  #Percorre o DNA de 1 em 1 ate a penultima letra
  for (i in 1:(n_letras - 1)) {
    #Pega a letra atual e a próxima
    primeira <- substr(dna, i, i)
    segunda <- substr(dna, i + 1, i + 1)
    # Se forem bases validas, soma 1 na posicao certa da matriz
    if (primeira %in% bases && segunda %in% bases) {
      matriz[primeira, segunda] <- matriz[primeira, segunda] + 1
    }
  }
  #Calcula a porcentagem
  total_pares <- sum(matriz)
  matriz_final <- (matriz / total_pares) * 100
  return(matriz_final)
}
#Funcao para gerar o grafico
gera_grafico <- function(dados, nome_virus) {
  plot_ly(x = colnames(dados), y = rownames(dados), z = dados, 
          type = "heatmap", colorscale = "Blues",
          text = round(dados, 2), texttemplate = "%{text}%") %>%
    layout(title = nome_virus, xaxis = list(title = "Base 2"), yaxis = list(title = "Base 1"))
}
matriz_covid <- calcula_pct(file.choose())
matriz_gripe <- calcula_pct(file.choose())
gera_grafico(matriz_covid, "SARS-CoV-2")
gera_grafico(matriz_gripe, "Influenza A")