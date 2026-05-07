set.seed(123)
simulacoes <- 10000
#Funcao simples calcular assimetria
calcula_assimetria <- function(dados) {
  n <- length(dados)
  media <- mean(dados)
  desvio <- sd(dados)
  assimetria <- sum((dados - media)^3) / (n * desvio^3)
  return(assimetria)
}
#Gerando variaveis uniformes
u1 <- runif(simulacoes)
u2 <- runif(simulacoes)
maximo_u <- pmax(u1, u2)
raiz_u <- sqrt(u2)
#Guarda os resultados da primeira tabela
tabela_transformacoes <- data.frame(
  variavel = c("Maximo", "Raiz"),
  media = c(mean(maximo_u), mean(raiz_u)),
  mediana = c(median(maximo_u), median(raiz_u)),
  assimetria = c(calcula_assimetria(maximo_u), calcula_assimetria(raiz_u))
)
#Gerando variaveis com metodo de rejeicao
y1 <- numeric(simulacoes)
y2 <- numeric(simulacoes)
contador <- 1
#Fica rodando ate conseguir preencher todas as posicoes necessarias
while (contador <= simulacoes) {
  v1 <- runif(1, -1, 1)
  v2 <- runif(1, -1, 1)
  w <- v1^2 + v2^2
  #So aceita fazer a conta se w for menor ou igual a 1
  if (w <= 1) {
    multiplicador <- sqrt(-2 * log(w) / w)
    y1[contador] <- v1 * multiplicador
    y2[contador] <- v2 * multiplicador
    contador <- contador + 1
  }
}
divisao_1 <- y1 / y2
divisao_2 <- y2 / y1
#Guarda os resultados da segunda tabela
tabela_divisoes <- data.frame(
  divisao = c("V1", "V2"),
  media = c(mean(divisao_1), mean(divisao_2)),
  mediana = c(median(divisao_1), median(divisao_2)),
  assimetria = c(calcula_assimetria(divisao_1), calcula_assimetria(divisao_2))
)
tabela_transformacoes
tabela_divisoes