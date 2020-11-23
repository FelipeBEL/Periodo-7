# Aluno: Felipe Alves Belisario
# Matricula: 11721BCC030

### 1 -

## a)

monte_c <- function(func, a, b, x, y) {
  intervals <- 10000
  entradas <- seq(from=a, to=b, by=b/intervals)
  n_random <- runif(n=intervals, min=x, max=y)
  curva_abaixo <- 0
  
  for (i in 1:intervals) {
    res <- func(entradas[[i]])
    if (n_random[[i]] < res) {
      curva_abaixo <- curva_abaixo + 1
    }
  }
  
  integration <- (curva_abaixo / length(entradas)) * (b - a) * (y - x)
  
  return(integration)
}

calculaIntegral <- function(x) {
  return (x/(1 + (2*x^2) + x^4))
}

resultado <- monte_c(calculaIntegral, 0, 6, 0, 0.5)


## b)

EstimativaIntegral <- function(n) {
  monte_carlo_result <- monte_c(calculaIntegral, 0, n, 0, 0.5)
  return (monte_carlo_result)
}

resultadoExemplo <- EstimativaIntegral(10)  # Exemplo

##########################################################

### 2 -

dados1 <- sample(x = 1:6, size = 100000, replace = TRUE)
dados2 <- sample(x = 1:6, size = 100000, replace = TRUE)
dados3 <- sample(x = 1:6, size = 100000, replace = TRUE)
dados4 <- sample(x = 1:6, size = 100000, replace = TRUE)
dados5 <- sample(x = 1:6, size = 100000, replace = TRUE)

## a)

sucessos <- 0
for(i in 1:100000){
  if(dados1[i] == 2 || dados2[i] == 2 || dados3[i] == 2 || dados4[i] == 2 || dados5[i] == 2){
    sucessos <- sucessos + 1
  }
}

sucessos <- sucessos/100000

## b)

soma <- (dados1 + dados2 + dados3 + dados4 + dados5)
soma14 <- soma[soma == 14]

sucesso <- length(soma14)/100000

## c)

sucessos <- 0
for(i in 1:100000){
  if(dados1[i] != dados2[i] && dados1[i] != dados3[i] && dados1[i] != dados4[i] && dados1[i] != dados5[i] &&
     dados2[i] != dados3[i] && dados2[i] != dados4[i] && dados2[i] != dados5[i] &&
     dados3[i] != dados4[i] && dados3[i] != dados5[i] &&
     dados4[i] != dados5[i]){
    
    sucessos <- sucessos + 1
    
  }
}

sucessos <- sucessos/100000

##########################################################

## 3 -


sequencia_bob <- c(0, 1, 0)
sequencia_patti <- c(0, 0, 1)

sucessos <- 0
fracassos <- 0


for(i in 1:100000){
  moedas_lancadas <- c()
  
  for(j in 1:100000){
    moedas_lancadas <- c(moedas_lancadas, sample(x = 0:1, size = 1, replace = TRUE))
    
    if(length(moedas_lancadas) >= 3){
      if(moedas_lancadas[j-2] == sequencia_patti[1] && moedas_lancadas[j-1] == sequencia_patti[2] && moedas_lancadas[j] == sequencia_patti[3]){
        sucessos <- sucessos + 1
        break
      }
      
      if(moedas_lancadas[j-2] == sequencia_bob[1] && moedas_lancadas[j-1] == sequencia_bob[2] && moedas_lancadas[j] == sequencia_bob[3]){
        fracassos <- fracassos + 1
        break
      }
    }
  }
}

sucessos <- sucessos/100000     ## Probabilidade Patti ganhar
fracassos <- fracassos/100000   ## Probabilidade Bob ganhar

