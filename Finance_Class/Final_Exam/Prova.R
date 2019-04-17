#Nome: Lucas de Queiroz Silva e Silva
#DRE: 115197960

library(rstudioapi)
library(boot)
library(mvtnorm)
library(PerformanceAnalytics)
library(stats)
library(readxl)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("portfolio.R")


START = "2013-05-01"
END = "2018-05-31"

codigos = c('ABEV3', 'LREN3', 'GRND3', 'HGTX3', 'EZTC3', 'EGIE3', 'WEGE3', 'CIEL3')

close.list = read_excel("close.xlsx")
close.df = data.frame(close.list)
close.df <- subset( close.df, select = -X__1 )

#Cria matriz de retornos continuamente compostos
ret.mat = apply(log(close.df), 2, diff)

#Cria a matriz de medias por ativo
mu.vec = apply(ret.mat, 2, mean)
mu.vec

#Cria a matriz de covariancia
sigma.mat = cov(ret.mat)


#Questao 1

# Rendimento mensal do ativo livre de risco
r.f = (1+0.0639)^(1/12) -1
r.f

#Desvio padrão dos ativos
sig.vec = apply(ret.mat, 2, sd)
sig.vec

#Matriz de Sharpe Ratio de cada um dos ativos
Sharpe.mat = (mu.vec - r.f)
Sharpe.mat


#Questao 2

#Grafico Risco x Retorno
sig.vec = append(sig.vec, 0)
mu.vec = append(mu.vec, r.f)
codigos = append(codigos, 'Atv Livre de Risco')
plot(sig.vec, mu.vec, xlab = expression(sigma[p]), ylab = expression(mu[p]), pch = 16, col = 'blue', xlim = c(min(sig.vec), max(sig.vec)+0.05), ylim = c(min(mu.vec), max(mu.vec)+0.02))
text(sig.vec, mu.vec, labels = codigos, pos = 4, cex = 0.6)


sig.vec = sig.vec[1:(length(sig.vec)-1)]
mu.vec = mu.vec[1:(length(mu.vec)-1)]
mu.vec
sig.vec
#Questao 3
#Utilizando portfolio.r
gmvp.port = globalMin.portfolio(er = mu.vec, cov.mat = sigma.mat)
gmvp.port

#Fazendo passo a passo
one.vec = rep(1, nrow(sigma.mat))
zero.vec = rep(0, nrow(sigma.mat))
A.m = rbind(cbind(2*sigma.mat, one.vec), c(t(one.vec), 0))
b = c(zero.vec, 1)
z.m = solve(a = A.m, b = b)
#Pesos dos ativos no portfolio
z.m

x.mvp = head(z.m, -1)
x.mvp
mu.mvp = t(x.mvp) %*% mu.vec
sig.mvp = sqrt(t(x.mvp) %*% sigma.mat %*% x.mvp)
points(sig.mvp, mu.mvp, pch = 16, col = 'magenta', cex = 1.5)
text(sig.mvp, mu.mvp, 'GMVP', pos = 2, cex = 0.7)


#Questao 4

A = rbind(cbind(2*sigma.mat, mu.vec, one.vec), c(t(mu.vec), 0, 0), c(t(one.vec), 0, 0))
A.inv = solve(A)
B.lren3 = c(zero.vec, mu.vec['LREN3'], 1)  

z.x = A.inv %*% B.lren3
z.x

x.vec = head(z.x, -2)
x.vec

mu.p.x = t(x.vec) %*% mu.vec
mu.p.x
sig.p.x = sqrt(t(x.vec) %*% sigma.mat %*% x.vec)
sig.p.x

points(x = sig.p.x, y = mu.p.x, pch = 16, col = 'green', cex = 1.5)
text(sig.p.x, mu.p.x, 'PortEfic. LREN3', pos = 2, cex = 0.7)


#Questao 5

mu.0 = mu.vec['LREN3']
sigma.inv = solve(sigma.mat)
m.vec = rowSums(sigma.inv)/sum(sigma.inv)
A = rbind(cbind(2*sigma.mat, mu.vec, one.vec), c(t(mu.vec), 0, 0), c(t(one.vec), 0, 0))
b = c(zero.vec, mu.0, 1)  
x.vec = head(solve(A, b), -2)

sig.m.x = t(m.vec) %*% sigma.mat %*% x.vec

alfa.vec = seq(from = 1, to = -2, by = -0.1)

n.pontos = length(alfa.vec)
mu.p.fronteira = rep(0, n.pontos)
sig.p.fronteira = rep(0, n.pontos)
sig2.mvp = sig.mvp^2
sig2.p.x = sig.p.x^2

for (i in 1:n.pontos) {
  alfa = alfa.vec[i]
  mu.p.fronteira[i] = alfa*mu.mvp + (1 - alfa)*mu.p.x
  sig2.p.fronteira = alfa^2*sig2.mvp + (1 - alfa)^2*sig2.p.x + 2*alfa*(1 - alfa)*sig.m.x
  sig.p.fronteira[i] = sqrt(sig2.p.fronteira)
}
points(sig.p.fronteira, mu.p.fronteira, pch = 16, col = 'yellow')


#Questao 6

dim = nrow(sigma.mat)
rf = r.f
sigma.inv = solve(sigma.mat)
one.vec = rep(1, dim)

#Pesos dos ativos
t.vec = sigma.inv %*% (mu.vec - rf * one.vec) / as.numeric(t(one.vec) %*% sigma.inv %*% (mu.vec - rf * one.vec))

mu.p.t = t(t.vec) %*% mu.vec
sig2.p.t = t(t.vec) %*% sigma.mat %*% t.vec
sig.p.t = sqrt(sig2.p.t)
points(sig.p.t, mu.p.t, pch = 16, col = 'red', cex=1.2)
text(sig.p.t, mu.p.t, 'tangente', pos = 4, cex = 0.7)



#Questao 7

abline(a = rf, b = (mu.p.t - rf)/sig.p.t, col = 'blue')


#Questao 8

vi = 100000
r.alvo = 0.03

#Valor investido em renda variável
x.t = r.alvo/sig.p.t
vrvar = x.t*vi

#Valor investido em renda fixa
x.f = 1 - x.t
vrfixa = x.f*vi

mu.p.e = rf + x.t*(mu.p.t - rf)
sig.p.e = x.t*sig.p.t
points(sig.p.e, mu.p.e, pch=16)
text(sig.p.e, mu.p.e, 'Invest Questao 8', pos = 4, cex = 0.7)



#Questao 9

vi=100000
VeR = (mu.p.e + sig.p.e*qnorm(0.05, mean = 0,sd = 1))*vi
VeR



#Questao 10
n=length(codigos)
n.obs = nrow(ret.mat)
rf = r.f
# bootstrap
n.boot = 1000
#Markowitz
plot(sig.p.fronteira, mu.p.fronteira, xlab = expression(sigma[p]), ylab = expression(mu[p]), pch = 16, col = 'blue', xlim = c(0, 0.4), ylim = c(0, 0.05))
# bootstrap
for (i in 1:n.boot) {
  boot.idx = sample(n.obs, replace=TRUE)
  ret.boot = ret.mat[boot.idx, ] 
  mu.boot = apply(ret.boot, 2, mean)
  cov.boot = cov(ret.boot) 
  sigma.inv = solve(cov.boot)
  dim = nrow(cov.boot)
  one.vec = rep(1, dim)
  t.vec = sigma.inv %*% (mu.boot - rf * one.vec) / as.numeric(t(one.vec) %*% sigma.inv %*% (mu.boot - rf * one.vec))
  mu.p.t = t(t.vec) %*% mu.boot
  sig2.p.t = t(t.vec) %*% cov.boot %*% t.vec
  sig.p.t = sqrt(sig2.p.t)
  points(sig.p.t, mu.p.t, pch = 16, col = 'red', cex=1.2)
}

