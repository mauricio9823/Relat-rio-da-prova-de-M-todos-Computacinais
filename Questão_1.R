set.seed(123)

repeticoes <- 10000
valores_n <- c(10, 100, 1000, 10000)

probabilidades <- numeric(length(valores_n))
indice <- 1

#Loop para testar cada tamanho de amostra n
for (n in valores_n) {
  
  #rbinom faz os lancamentos de todas as repeticoes de uma vez
  caras_maria <- rbinom(repeticoes, size = n, prob = 0.5)
  caras_victor <- rbinom(repeticoes, size = n - 1, prob = 0.5)
  
  #Soma as vezes em que Maria teve estritamente mais caras
  vitorias <- sum(caras_maria > caras_victor)
  
  #Guarda o valor calculado no vetor
  probabilidades[indice] <- vitorias / repeticoes
  indice <- indice + 1
}

#Junta os valores em uma tabela simples
tabela_resultados <- data.frame(
  tamanho_n = valores_n,
  probabilidade_maria = probabilidades
)
tabela_resultados