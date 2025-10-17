
mod_aroma <- lm(qualidade ~ aroma, data = wine)
summary(mod_aroma)

mod_aroma1 <- lm(log(qualidade) ~ aroma, data = wine)
summary(mod_aroma1)

mod_aroma2 <- lm(log(qualidade) ~ log(aroma), data = wine)
summary(mod_aroma2)

mod_aroma3 <- lm(sqrt(qualidade) ~ aroma, data = wine)
summary(mod_aroma3)

mod_aroma4 <- lm(log(qualidade) ~ aroma, data = wine)
summary(mod_aroma4)

par(mfrow = c(2, 3))

plot(aroma,qualidade, pch = 16)
plot(log(aroma), qualidade, pch = 16)
plot(aroma, log(qualidade), pch = 16)
plot(log(aroma), log(qualidade), pch = 16)
plot(aroma, sqrt(qualidade), pch = 16)

# suposições

## Normalidade 

par(mfrow = c(1, 1))

envelope_LR(mod_aroma,  main.title = "") # dois pontos pra fora
envelope_LR(mod_aroma1,  main.title = "") # três pontos pra fora e um no limite
envelope_LR(mod_aroma2,  main.title = "") # setes pontos pra fora e alguns no limite (pior ajuste - descartado)
envelope_LR(mod_aroma3,  main.title = "") # dois pontos pra fora
envelope_LR(mod_aroma4,  main.title = "") # seis pontos pra fora (descartado)

## Homogeneidade
tsi <- rstudent(mod_aroma);a <- max(tsi);b <- min(tsi)
plot(fitted(mod_aroma),tsi,xlab="Valor Ajustado",ylab="Resíduo Studentizado", ylim=c(b-1,a+1), pch=16)
abline(h = c(-3, -2, 0, 2, 3),
       col = c("darkred", "red", "blue", "red", "darkred"),
       lty = c(3, 2, 2, 2, 3))
identify(fitted(mod_aroma),tsi)
#a variância parece diminuir no final do gráfico, formato de funil. 
# há um ponto a ser investigado, abaixo de -2 (observação 30)

tsi <- rstudent(mod_aroma1);a <- max(tsi);b <- min(tsi)
plot(fitted(mod_aroma1),tsi,xlab="Valor Ajustado",ylab="Resíduo Studentizado", ylim=c(b-1,a+1), pch=16)
abline(h = c(-3, -2, 0, 2, 3),
       col = c("darkred", "red", "blue", "red", "darkred"),
       lty = c(3, 2, 2, 2, 3))
identify(fitted(mod_aroma1),tsi)
#a variância parece diminuir no final do gráfico, formato de funil. 
# há dois pontos a serem investigados, abaixo de -2 (observações 20 e 30)

tsi <- rstudent(mod_aroma3);a <- max(tsi);b <- min(tsi)
plot(fitted(mod_aroma3),tsi,xlab="Valor Ajustado",ylab="Resíduo Studentizado", ylim=c(b-1,a+1), pch=16)
abline(h = c(-3, -2, 0, 2, 3),
       col = c("darkred", "red", "blue", "red", "darkred"),
       lty = c(3, 2, 2, 2, 3))
identify(fitted(mod_aroma3),tsi)
# a variância parece diminuir no final do gráfico, formato de funil. 
# há dois pontos a serem investigados, abaixo de -2 (observações 20 e 30)


## Não correlação dos erros
tsi <- rstudent(mod_aroma);a <- max(tsi);b <- min(tsi)
plot(tsi, pch=16, xlab="Índice", ylab="Resíduo Studentizado",ylim=c(b-1,a+1))
abline(h = c(-3, -2, 0, 2, 3),
       col = c("darkred", "red", "blue", "red", "darkred"),
       lty = c(3, 2, 2, 2, 3))
identify(tsi)
# observação 30 abaixo de -2
dwtest(mod_aroma, alternative = "two.sided") # há autocorrelação entre os erros

tsi <- rstudent(mod_aroma1);a <- max(tsi);b <- min(tsi)
plot(tsi, pch=16, xlab="Índice", ylab="Resíduo Studentizado",ylim=c(b-1,a+1))
abline(h = c(-3, -2, 0, 2, 3),
       col = c("darkred", "red", "blue", "red", "darkred"),
       lty = c(3, 2, 2, 2, 3))
identify(tsi)
# observações 20 e 30 abaixo de -2
   
dwtest(mod_aroma1, alternative = "two.sided")

tsi <- rstudent(mod_aroma4);a <- max(tsi);b <- min(tsi)
plot(tsi, pch=16, xlab="Índice", ylab="Resíduo Studentizado",ylim=c(b-1,a+1))
abline(h = c(-3, -2, 0, 2, 3),
       col = c("darkred", "red", "blue", "red", "darkred"),
       lty = c(3, 2, 2, 2, 3))
identify(tsi)
# observações 20 e 30 abaixo de -2

dwtest(mod_aroma4, alternative = "two.sided")

# para todas as transformações, há desvios graves em relação à homocedasticidade e não correlação dos resíduos
# portanto, esta variável não é a melhor preditora para a qualidade do vinho

# ---------------------------------------------------------------------------------------------------------
mod_corpo <- lm(qualidade ~ corpo, data = wine)
summary(mod_corpo)

mod_corpo1 <- lm(log(qualidade) ~ corpo, data = wine)
summary(mod_corpo1)

mod_corpo2 <- lm(qualidade ~ log(corpo), data = wine)
summary(mod_corpo2)

mod_corpo3 <- lm(log(qualidade) ~ log(corpo), data = wine)
summary(mod_corpo3)

mod_corpo4 <- lm(sqrt(qualidade) ~ corpo, data = wine)
summary(mod_corpo4)

mod_corpo5 <- lm(qualidade ~ sqrt(corpo), data = wine)
summary(mod_corpo5)

par(mfrow = c(2, 3))

plot(corpo,qualidade, pch = 16)
plot(corpo, log(qualidade), pch = 16)
plot(log(corpo), qualidade, pch = 16)
plot(log(corpo), log(qualidade), pch = 16)
plot(corpo, sqrt(qualidade), pch = 16)
plot(sqrt(corpo), qualidade, pch = 16)

## Normalidade 

par(mfrow = c(1, 1))

envelope_LR(mod_corpo,  main.title = "") # três pontos pra fora e três no limite (bom)

envelope_LR(mod_corpo1,  main.title = "") # sete pontos pra fora, ajuste ruim

envelope_LR(mod_corpo2,  main.title = "") # dois pontos pra fora e um no limite (bom)

envelope_LR(mod_corpo3,  main.title = "") # nove pontos pra fora e um no limite, ajuste ruim

envelope_LR(mod_corpo4,  main.title = "") # dois pontos pra fora e vários no limite, ajuste ruim

envelope_LR(mod_corpo5,  main.title = "") # dois pra fora e três no limite
