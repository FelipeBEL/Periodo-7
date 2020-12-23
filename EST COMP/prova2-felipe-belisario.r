## Aluno:   Felipe Alves Belisário (11721BCC030)

## 1 -

# a)

calculaProbInversa <- function(n){
  x <- c()
  
  for(i in 1:n){
    u <- runif(1, 0, 1)
    x[i] <- -50*log(1 - u)
  }
  
  return (x)
  
}

n <- 10000
valoresGerados <- calculaProbInversa(n)   # Valores gerados para n = 10000
print(valoresGerados)


# b)

probabilidade <- length(valoresGerados[valoresGerados > 6]) / n     # Estimativa de P(T > 6)
print(probabilidade)

# c)

probabilidadeT1 <- length(valoresGerados[valoresGerados > 18]) / n  # Estimativa de P(T > 18)
probabilidadeT2 <- length(valoresGerados[valoresGerados > 12]) / n  # Estimativa de P(T > 12)

probabilidadeIntersecao <- probabilidadeT1 * probabilidadeT2        # Estimativa de P(T1 intersecao T2)

probabilidadeT1DadoT2 <- probabilidadeIntersecao / probabilidadeT2  # Estimativa de P(T1 | T2)
print(probabilidadeT1DadoT2)

######################################

## 2 -

# a)

geraValoresX <- function(n){
  x <- c()
  
  for(i in 1:n){
    soma <- 0
    qtdSorteios <- 0
    
    repeat{
      u <- runif(1, 0, 1)
      soma <- soma + u
      qtdSorteios <- qtdSorteios + 1
      
      if(soma > 1){
        break
      }
    }

    x[i] <- qtdSorteios
  }
  
  return (x)
  
}

valoresGerados <- geraValoresX(10000)     # Valores gerados para n = 10000
print(valoresGerados)


# b)

calculaEsperanca <- function(x){
  soma <- 0
  
  for(i in i:length(x)){
    soma <- sum(x[i] * (length(x[x==x[i]]) / length(x)))   # Estimativa de E[X]
  }
  
  return (soma)
}


esperanca <- calculaEsperanca(valoresGerados)

######################################

## 3 -

# a)

irisTable <- read.table("iris.txt", sep = ",", header = TRUE)

# b)

somaLargura <- sum(irisTable$largura_petala, na.rm = TRUE)
somaComprimento <- sum(irisTable$comprimento_petala)

largMedia <- somaLargura/length(irisTable$largura_petala)
compMedio <- somaComprimento/length(irisTable$comprimento_petala)

desvioComp <- sd(irisTable$comprimento_petala,compMedio)
desvioLarg <- sd(irisTable$largura_petala,largMedia)


print(largMedia)    # Media da largura
print(compMedio)    # Media do comprimento
print(desvioLarg)   # Desvio padrao da largura
print(desvioComp)   # Desvio padrao do comprimento

# c)

plot((irisTable$largura_petala), (irisTable$comprimento_petala))

# d)

correlacao <- cor(irisTable$largura_petala,irisTable$comprimento_petala,use = "complete.obs")
print(correlacao)

# A correlacao se encontra maior que 0.9, isso significa que existe uma correlacao muito forte

# e)

reta <- lm(irisTable$largura_petala ~ irisTable$comprimento_petala)
print(reta)

# f)

alteracaoLargura <- 0.5 * 0.4171 
print(alteracaoLargura)

# A variacao do comprimento da petala pode ser calculado substituindo o X na equação da reta já que a cada valor multiplicado no coeficiente angular
# aumentado-se em 0.4171 cm a largura da petala (para esse caso)

# g)

coefDeterminacao <- correlacao ** 2
print(coefDeterminacao)

# h)

# A largura de 2.8 é maior que o valor maximo das larguras especificado nas amostras,
# logo nao se pode garantir que o calculo dara certo para um valor fora da variancia do conjunto de dados informado

