#Nome: Lucas de Queiroz Silva e Silva
#DRE: 115197960

library(rstudioapi)
library(mvtnorm)
library(PerformanceAnalytics)
library(stats)
library(readxl)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


START = "2015-06-01"
END = "2018-06-01"

codigos = c("PETR4.SA", "VALE3.SA", "ITUB4.SA", "BBAS3.SA")

close.list = read_excel("close.xlsx")
close.df = data.frame(close.list)
close.df <- subset( close.df, select = -X__1 )

#Cria matriz de retornos continuamente compostos
ret.mat = apply(log(close.df), 2, diff)

#Cria a matriz de medias por ativo
mu.vec = apply(ret.mat, 2, mean)
mu.vec

#Cria a matriz de covariância
sigma.mat = cov(ret.mat)

#Testa se está simulando corretamente
lensimul = 800
ret.sim = rmvnorm(lensimul, mean = mu.vec, sigma = sigma.mat)
colnames(ret.sim) = codigos
plot(ret.sim[, 'PETR4.SA'], type = 'l', lwd = 2, col = 'blue', main = 'PETR4 MC')
lines(ret.mat[, 'PETR4.SA'], type = "l", lwd = 2, col = 'red')
cor(ret.sim)
cor(ret.mat)

#Simulação 
nsimul = 100000
pinicial = close.df[1,]
obs = c()
for (i in 1:nsimul) {
  ret.sim = rmvnorm(753, mean = mu.vec, sigma = sigma.mat)
  
  #Sendo a matriz de retornos simulados por Monte Carlo composta por retornos continuamente compostos, somando-os obtem-se o retorno continuamente composto do período desejado.
  retobserv =  apply(ret.sim[1:126,], 2, sum)
  retobserv <- rbind(retobserv, apply(ret.sim[1:250,], 2, sum))
  retobserv <- rbind(retobserv, apply(ret.sim[1:378,], 2, sum))
  retobserv <- rbind(retobserv, apply(ret.sim[1:502,], 2, sum))
  retobserv <- rbind(retobserv, apply(ret.sim[1:629,], 2, sum))
  retobserv <- rbind(retobserv, apply(ret.sim[1:753,], 2, sum))
  
  #Matriz de retornos simples por período observado
  retobserv = (exp(retobserv))
  retobserv
  
  #Adiciona 0 ao vetor de observações e, caso haja pagamento de cupom, remove-o e adiciona o número da observação em que ele é pago.
  obs = append(obs, 0)
  j=1
  for(j in 1:6) {
    if(all(retobserv[j,]>1)){
      obs = obs[1:length(obs)-1]
      obs = append(obs, j)
      break()
      }
    
  }
}

#O data frame probs é composto pelos índices de cada observação(obs), sendo 1 o da 1ª observação, 2 da 2ª... e 0 caso o COE seja encerrado sem pagamento do cupom.
probs = as.data.frame(table(obs))
Probabilidade = (probs[,2]/nsimul)*100
Indices = c("Encerra sem pagar cupom", "Encerra e paga na 1ª observacao", "Encerra e paga na 2ª observacao", "Encerra e paga na 3ª observacao", "Encerra e paga na 4ª observacao", "Encerra e paga na 5ª observacao", "Encerra e paga na 6ª observacao")
ResultadoEsperado = cbind(Indices, Probabilidade)
ResultadoEsperado
