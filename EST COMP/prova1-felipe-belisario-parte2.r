### 1 -

# Desculpe professor, não consegui =(

### 2 -

## a)

calculaPosicaoLuke <- function(tipo){
  sucessos <- 0
  posicaoLuke <- 0
  moedas <- sample(x = 0:1, size = 100000, replace = TRUE)
  
  for(nro_jogadas in 1:100000){
    
    if(moedas[nro_jogadas] == 0){
      posicaoLuke <- posicaoLuke - 1
    }
    
    if(moedas[nro_jogadas] == 1){
      posicaoLuke <- posicaoLuke + 1
    }
    
    if(tipo == "Pares"){
      if(posicaoLuke == 0 && (nro_jogadas %% 2) == 0){
        sucessos <- sucessos + 1
      }
    }
    
    if(tipo == "Impares"){
      if(posicaoLuke == 0 && (nro_jogadas %% 2) != 0){
        sucessos <- sucessos + 1
      }
    }
    
  }
  
  return (sucessos/100000)
}

probPares <- calculaPosicaoLuke("Pares")
probImpares <- calculaPosicaoLuke("Impares")

## Resposta: Concordo com Yoda, pois simulando para 100000 lançamentos da moeda a probabilidade de Luke voltar para
## a origem com uma quantidade par de lançamentos é abaixo de 1% porém ainda existe, enquanto que para uma quantidade
## impar a simulação sempre tem como resposta uma possibilidade de 0% de se fazer esse retorno. Isso se deve a uma
## propriedade dos numeros impares em que a soma de dois numeros impares é sempre par, ja que sempre que Luke andar
## certa quantidade para um lado ele tera que andar essa mesma quantidade para voltar para a origem, então o numero
## de passos seria sempre 2*n considerando n o numero da posição atual.


## b)

calculaPosicaoLukePares <- function(qtd_jogadas){
  sucessos <- 0
  
  for(nro_inicios in 1:100000){
    posicaoLuke <- 0
    
    for(nro_jogadas in 1:qtd_jogadas){
      moeda <- sample(x = 0:1, size = 1, replace = TRUE)
      
      if(moeda == 0){
        posicaoLuke <- posicaoLuke - 1
      }
      
      if(moeda == 1){
        posicaoLuke <- posicaoLuke + 1
      }
      
      if(posicaoLuke == 0 && nro_jogadas == qtd_jogadas){
        sucessos <- sucessos + 1
      }
    }
    
  }
  
  return (sucessos/100000)
}

# b.a)

prob4 <- calculaPosicaoLukePares(4)

# b.b)

prob6 <- calculaPosicaoLukePares(6)

# b.c)

prob10 <- calculaPosicaoLukePares(10)

# b.d)

prob20 <- calculaPosicaoLukePares(20)

