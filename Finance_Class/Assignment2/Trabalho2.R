#Aluno : Lucas de Queiroz Silva e Silva
#DRE : 115197960
library(PerformanceAnalytics)
library(stats)

# I

set.seed(1)
#1)
amostra = c(3, 3, 6, 7, 7, 10, 10, 10, 11, 13, 30)

#a)
media = mean(amostra)
media
#b)
mediana = median(amostra)
mediana
#c)
variancia = var(amostra)*(length(amostra)-1)/length(amostra)
#A variancia implicita no R é a variancia amostral(1/(n-1)*somatorio das diferencas ao quadrado entre media e elementos da amostra).
#Para converter basta multiplicar por (n-1)/n.
variancia

#d)
desvpad = sd(amostra)
desvpad

#e)
quantis = quantile(amostra)
quantis

#2)
normal = rnorm(10000, 0.5, 0.5)

#a)
medianorm = mean(normal)
medianorm

#b)
variancianorm = var(normal)*(length(normal)-1)/length(normal)
variancianorm

#c)
desvpadnorm = sd(normal)
desvpadnorm

# II
ativo1 = rnorm(10000, 0.12, 1.234)
ativo2 = rnorm(10000, 0.32, 1.272)

#3)
plot(density(ativo1), type="l", col="blue")
lines(density(ativo2), type="l", col="red")
abline(v=0.12, col="blue")
abline(v=(0.12+1.234),col="blue")
abline(v=(0.12-1.234), col="blue")
abline(v=0.32, col="red")
abline(v=(0.32+1.272),col="red")
abline(v=(0.32-1.272), col="red")

#4)
summary(ativo1)
summary(ativo2)
kurtosis(ativo1)
kurtosis(ativo2)
#O Ativo 1 é mais arriscado pois, apesar do excesso de curtose de ambos os ativos ser proximo de 0(curtose dos ativos proxima da curtose da normal), a media do ativo 1 está mais próxima de 0 e, consequentemente, a ocorrencia de rendimentos negativos é maior.

#5)
medias = c(0.12, 0.32)
desvpads = c(1.234, 1.272)
plot(medias, desvpads, type="p", xlab = "media", ylab = "desvio padrão", xlim = c(0, 0.5), ylim = c(1, 1.5))

#6)
var1 = qnorm(0.01, 0.12, 1.234)*1000
var2 = qnorm(0.01, 0.32, 1.272)*1000
var1
var2
#2750,71 e 2639,11 reais em risco, respectivamente

#7)Significa que, para a confiança especificada(1%), a perda maxima provavel dos ativos é de respectivamente R$2750,71 e R$2639,11.

#8)A distribuição normal pode retornar um valor do ativo negativo ao final do período e o retorno simples deve sempre ser maior que -1.

#9)
#rt(12) = N(12*0.014, 12*0.05)
retcc12 = rnorm(12, 12*0.014, 12*0.05)
valoresperado = mean(retcc12)
desviopadrao = sd(retcc12)

#10)
tstudent = rt(200, 2)
distnormal = rnorm(200,0,1)
curtosenorm = kurtosis(distnormal)+3
skewnessnorm = skewness(distnormal)
curtosestudent = kurtosis(tstudent)+3
skewnessstudent = skewness(tstudent)

#11)
lognormal = rlnorm(200,0,1)
distnormal = rnorm(200,0,1)
curtosenorm = kurtosis(distnormal)+3
skewnessnorm = skewness(distnormal)
curtoselognormal = kurtosis(lognormal)+3
skewnesslognormal = skewness(lognormal)

#12)
xa = 0.33
xb = 0.67
media1 = 0.12
media2 = 0.32
desvpad1 = 1.234
desvpad2 = 1.272
#a)
retport = xa*media1 + xb*media2
retport

#b)
cov12 = 0.3
varport = (xa^2)*(desvpad1^2) + (xb^2)*(desvpad2^2) + 2*xa*xb*cov12
varport

#c)
desvpadport = sqrt(varport)
desvpadport

#13)Apesar de possuir valor esperado menor que o ativo 2, o portfolio possui consideravelmente menos risco haja vista que seu desvio padrão é pelo menos 17% menor que o dos ativos individuais. Segue um plot risco-retorno dos ativos e do portfolio.
medias = c(0.12, 0.32)
desvpads = c(1.234, 1.272)
plot(medias, desvpads, type="p", xlab = "media", ylab = "desvio padrão", xlim = c(0, 0.5), ylim = c(1, 1.5), col = "blue")
lines(retport, desvpadport, type = "p", col = "red")
legend("topleft", legend=c("ativos", "portfolio"), col=c("blue", "red"), lty=1:2, cex=0.8)
