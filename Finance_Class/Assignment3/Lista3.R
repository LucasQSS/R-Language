#Nome: Lucas de Queiroz Silva e Silva
#DRE: 115197960

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(rstudioapi)
library(PerformanceAnalytics)
library(stats)

START = "2010-05-14"
END = "2018-05-14"

codigos = c("ABEV3.SA", "EMBR3.SA", "PETR4.SA", "RADL3.SA")

close.df = read.csv(file = paste(codigos[1], "csv", sep="."), 
                    header=TRUE, sep=',', stringsAsFactors=FALSE)

for(i in 2:length(codigos)) {
  aux.df = read.csv(file = paste(codigos[i], "csv", sep="."), 
                    header=TRUE, sep=',', stringsAsFactors=FALSE)
  close.df = cbind(close.df, aux.df[,2])
}

colunas.names = cbind("Date", "ABEV3.SA", "EMBR3.SA", "PETR4.SA", "RADL3.SA")
colnames(close.df) = colunas.names
rownames(close.df) = close.df$Date
close.df$Date = NULL
head(close.df)

#1)

n = nrow(close.df)
ano = seq(as.Date("2010/05/15"), by = "month", length.out = n-1)
ret.df = log(close.df[2:n,]/close.df[1:n-1,])
plot(ano, ret.df[, 'PETR4.SA'], type = 'l', lwd = 2, ylab = 'PETR4', main="Retornos Mensais CC PETR4")
plot(ano, ret.df[, 'ABEV3.SA'], type = 'l', lwd = 2, ylab = 'ABEV3', main="Retornos Mensais CC ABEV3")
plot(ano, ret.df[, 'EMBR3.SA'], type = 'l', lwd = 2, ylab = 'EMBR3', main="Retornos Mensais CC EMBR3")
plot(ano, ret.df[, 'RADL3.SA'], type = 'l', lwd = 2, ylab = 'RALD3', main="Retornos Mensais CC RADL3")


#2)
attach(mtcars)
par(mfrow=c(2,2))
retabev3 = (close.df[2:n,1]/close.df[1,1])
retembr3 = (close.df[2:n,2]/close.df[1,2])
retpetr4 = (close.df[2:n,3]/close.df[1,3])
retradl3 = (close.df[2:n,4]/close.df[1,4])
plot(ano, retpetr4, type = 'l', lwd = 2, ylab = 'PETR4', main="Retorno Acumulado PETR4")
plot(ano, retabev3, type = 'l', lwd = 2, ylab = 'ABEV3', main="Retorno Acumulado ABEV3")
plot(ano, retembr3, type = 'l', lwd = 2, ylab = 'EMBR3', main="Retorno Acumulado EMBR3")
plot(ano, retradl3, type = 'l', lwd = 2, ylab = 'RALD3', main="Retorno Acumulado RADL3")
layout(1)

#3)

retsimp.df = close.df[n,]/close.df[1,] -1

#claramente os ativos mais rentaveis são disparadamente ABEV3 E RALD3, que tiveram retorno igual a 6.346405 e 5.561929, respectivamente, seguidos de EMBR3 que possui retorno igual a 1.642752 e por fim PETR4 com retorno 0.0405508(o retorno real de PETR4 foi negativo devido a inflação no período ser superior ao retorno do investimento.)

#4)

#PETR4
attach(mtcars)
par(mfrow=c(2,2))
hist(ret.df[, 'PETR4.SA'], col = 'slateblue1', freq = F, main = 'Histograma Retornos CC PETR4')
plot(density(ret.df[, 'PETR4.SA']), type = 'l', lwd = 2, ylab = 'PETR4', main="Retornos Mensais CC PETR4")
boxplot(ret.df[, 'PETR4.SA'], col = 'slateblue1', ylab = 'retorno mensal cc', main = 'Boxplot dos retornos mensais CC de PETR4')
qqnorm(ret.df[, 'PETR4.SA'], col = 'blue', xlab = 'Quantis teorico PETR4')
qqline(ret.df[, 'PETR4.SA'])
layout(1)


#ABEV3
attach(mtcars)
par(mfrow=c(2,2))
hist(ret.df[, 'ABEV3.SA'], col = 'slateblue1', freq = F, main = 'Histograma Retornos CC ABEV3')
plot(density(ret.df[, 'ABEV3.SA']), type = 'l', lwd = 2, ylab = 'PETR4', main="Retornos Mensais CC ABEV3")
boxplot(ret.df[, 'ABEV3.SA'], col = 'slateblue1', ylab = 'retorno mensal cc', main = 'Boxplot dos retornos mensais CC de ABEV3')
qqnorm(ret.df[, 'ABEV3.SA'], col = 'blue', xlab = 'Quantis teorico ABEV3')
qqline(ret.df[, 'ABEV3.SA'])
layout(1)


#RADL3
attach(mtcars)
par(mfrow=c(2,2))
hist(ret.df[, 'RADL3.SA'], col = 'slateblue1', freq = F, main = 'Histograma Retornos CC RADL3')
plot(density(ret.df[, 'RADL3.SA']), type = 'l', lwd = 2, ylab = 'PETR4', main="Retornos Mensais CC RADL3")
boxplot(ret.df[, 'RADL3.SA'], col = 'slateblue1', ylab = 'retorno mensal cc', main = 'Boxplot dos retornos mensais CC de RADL3')
qqnorm(ret.df[, 'RADL3.SA'], col = 'blue', xlab = 'Quantis teorico RADL3')
qqline(ret.df[, 'RADL3.SA'])
layout(1)


#EMBR3
attach(mtcars)
par(mfrow=c(2,2))
hist(ret.df[, 'EMBR3.SA'], col = 'slateblue1', freq = F, main = 'Histograma Retornos CC EMBR3')
plot(density(ret.df[, 'EMBR3.SA']), type = 'l', lwd = 2, ylab = 'PETR4', main="Retornos Mensais CC EMBR3")
boxplot(ret.df[, 'EMBR3.SA'], col = 'slateblue1', ylab = 'retorno mensal cc', main = 'Boxplot dos retornos mensais CC de EMBR3')
qqnorm(ret.df[, 'EMBR3.SA'], col = 'blue', xlab = 'Quantis teorico EMBR3')
qqline(ret.df[, 'EMBR3.SA'])
layout(1)


#5)
#função apply dando problema

#petr4
valorespetr4 = c(mean(ret.df[, 'PETR4.SA']), sd(ret.df[, 'PETR4.SA']), var(ret.df[, 'PETR4.SA']), skewness(ret.df[, 'PETR4.SA']), kurtosis(ret.df[, 'PETR4.SA']))
summarypetr4 = summary(ret.df[, 'PETR4.SA'])

#abev3
valoresabev3 = c(mean(ret.df[, 'ABEV3.SA']), sd(ret.df[, 'ABEV3.SA']), var(ret.df[, 'ABEV3.SA']), skewness(ret.df[, 'ABEV3.SA']), kurtosis(ret.df[, 'ABEV3.SA']))
summaryabev3 = summary(ret.df[, 'ABEV3.SA'])

#embr3
valoresembr3 = c(mean(ret.df[, 'EMBR3.SA']), sd(ret.df[, 'EMBR3.SA']), var(ret.df[, 'EMBR3.SA']), skewness(ret.df[, 'EMBR3.SA']), kurtosis(ret.df[, 'EMBR3.SA']))
summaryembr3 = summary(ret.df[, 'EMBR3.SA'])

#radl3
valoresradl3 = c(mean(ret.df[, 'RADL3.SA']), sd(ret.df[, 'RADL3.SA']), var(ret.df[, 'RADL3.SA']), skewness(ret.df[, 'RADL3.SA']), kurtosis(ret.df[, 'RADL3.SA']))
summaryradl3 = summary(ret.df[, 'RADL3.SA'])


#6)
vini = 100
#petr4
qpetr4 = quantile(ret.df[, 'PETR4.SA'], probs = c(0.01, 0.05))
Varpetr4 = vini*c(exp(qpetr4[1])-1, exp(qpetr4[2])-1)
#Para um VaR de 1% e 5%, temos R$24,55 e R$20,00 em risco, respectivamente

#abev3
qabev3 = quantile(ret.df[, 'ABEV3.SA'], probs = c(0.01, 0.05))
Varabev3 = vini*c(exp(qabev3[1])-1, exp(qabev3[2])-1)
#Para um VaR de 1% e 5%, temos R$9,76 e R$6,57 em risco, respectivamente


#embr3
qembr3 = quantile(ret.df[, 'EMBR3.SA'], probs = c(0.01, 0.05))
Varembr3 = vini*c(exp(qembr3[1])-1, exp(qembr3[2])-1)
#Para um VaR de 1% e 5%, temos R$15,40 e R$11,70 em risco, respectivamente


#radl3
qradl3 = quantile(ret.df[, 'RADL3.SA'], probs = c(0.01, 0.05))
Varradl3 = vini*c(exp(qradl3[1])-1, exp(qradl3[2])-1)
#Para um VaR de 1% e 5%, temos R$12,60 e R$10,70 em risco, respectivamente


#7)

