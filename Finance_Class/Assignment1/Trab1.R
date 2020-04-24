#Aluno : Lucas de Queiroz Silva e Silva
#DRE : 115197960

#Escolhendo diretório:
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Cria dataframe
ciel3.df <- read.csv(file="CIEL3.SA.csv", header=TRUE, sep=",", stringsAsFactors=FALSE)

#Testa se dataframe foi criado corretamente
ciel3.df

#Questão 1)a)
dataini = "2013-03-01"
datafin = "2013-04-01"
index1 = which(ciel3.df$Date==dataini)
index2 = which(ciel3.df$Date==datafin)

valor1 = ciel3.df[index1, 2]
valor2 = ciel3.df[index2, 2]

retsimp = (valor2/valor1)-1
retsimp

#b)

Vi=10000

Vf = Vi*(retsimp+1)
Vf


#2)a)

retcc = log(retsimp+1)
retcc


#b)

retsimp2 = exp(retcc)-1
retsimp2


#3)

m=12
n=1
retacm = (1 + (retsimp/m))^(n*m)
retacm


#4)

retacc = 12*retcc
retacc


#5)a)

dataini = "2013-03-01"
datafin = "2014-03-01"
index1 = which(ciel3.df$Date==dataini)
index2 = which(ciel3.df$Date==datafin)

valor1 = ciel3.df[index1, 2]
valor2 = ciel3.df[index2, 2]

retsimp5 = (valor2/valor1)-1
retsimp5


#b)

Vf6b = Vi*(retsimp5 + 1)
Vf6b


#c) - Word

#6)a)

retacc6 = log(retsimp5 + 1)
retacc6


#b) - Word


#c)

retsimp6 = (exp(retacc6) - 1)
retsimp6


#7)a) e b)
Valores = ciel3.df[,2]
Valores
Rendimentos <- c(0)
Soma = 0
for (i in 2:length(Valores)){
  Rendimentos[i] <- (Valores[i]/Valores[i-1])-1
  Soma = Soma+Rendimentos[i]
}
Rendimentos
ciel3.df.7 = cbind(ciel3.df, Rendimentos)
ciel3.df.7

Media = Soma/(length(Rendimentos)-1)
Media


#7)c)

plot(Rendimentos[2:length(Rendimentos)], type="l", col="blue", xlab = "Datas", ylab="Rendimento", main="Retornos mensais de ciel3.sa")
abline(h=Media)

#8)a)

Montante = c(1)
for (i in 1:length(Rendimentos)){
  Montante[i+1]=Montante[i]*(1+Rendimentos[i])
}
Montante
Resposta8a = Montante[length(Montante)]
Resposta8a

#b)

plot(Montante, type="l", col="blue", xlab = "Datas", ylab="Rendimento", main="Retornos mensais de ciel3.sa")


