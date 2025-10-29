
mod_aroma <- lm(qualidade ~ aroma, data = wine)
summary(mod_aroma) # rse: 1.466

mod_aroma1 <- lm(log(qualidade) ~ aroma, data = wine)
summary(mod_aroma1) # rse: 0.1288

mod_aroma2 <- lm(log(qualidade) ~ log(aroma), data = wine)
summary(mod_aroma2) # rse: 0.1301

mod_aroma3 <- lm(sqrt(qualidade) ~ aroma, data = wine)
summary(mod_aroma3) # 0.2161

mod_aroma4 <- lm(qualidade ~ log(aroma), data = wine)
summary(mod_aroma4) # 1.92

par(mfrow = c(2, 3))

plot(aroma,qualidade, pch = 16)
plot(log(aroma), qualidade, pch = 16)
plot(aroma, log(qualidade), pch = 16)
plot(log(aroma), log(qualidade), pch = 16)
plot(aroma, sqrt(qualidade), pch = 16)

# suposições

## Normalidade 

par(mfrow = c(1, 1))

envelope_LR(mod_aroma,  main.title = "") # dois pontos pra fora, ajuste bom
qqPlot(mod_aroma) # nenhum pra fora

envelope_LR(mod_aroma1,  main.title = "") # três/quatro pontos pra fora e um no limite
qqPlot(mod_aroma1) # um pra fora

envelope_LR(mod_aroma2,  main.title = "") # quatro pontos pra fora e alguns no limite (pior ajuste - descartado)
qqPlot(mod_aroma2) # dois pra fora

envelope_LR(mod_aroma3,  main.title = "") # dois pontos pra fora
qqPlot(mod_aroma3) # um pra fora

envelope_LR(mod_aroma4,  main.title = "") # três pontos pra fora 
qqPlot(mod_aroma4) # três pra fora

## Homogeneidade
tsi <- rstudent(mod_aroma);a <- max(tsi);b <- min(tsi)
plot(fitted(mod_aroma),tsi,xlab="Valor Ajustado",ylab="Resíduo Studentizado", ylim=c(b-1,a+1), pch=16)
abline(h = c(-3, -2, 0, 2, 3),
       col = c("darkred", "red", "blue", "red", "darkred"),
       lty = c(3, 2, 2, 2, 3))
identify(fitted(mod_aroma),tsi)

gqtest(mod_aroma, fraction=1/3, order.by=model.frame(mod_aroma)$aroma, alternative="two.sided")

#a variância parece diminuir no final do gráfico, formato de funil. 
# há um ponto a ser investigado, abaixo de -2 (observação 30)

tsi <- rstudent(mod_aroma1);a <- max(tsi);b <- min(tsi)
plot(fitted(mod_aroma1),tsi,xlab="Valor Ajustado",ylab="Resíduo Studentizado", ylim=c(b-1,a+1), pch=16)
abline(h = c(-3, -2, 0, 2, 3),
       col = c("darkred", "red", "blue", "red", "darkred"),
       lty = c(3, 2, 2, 2, 3))
identify(fitted(mod_aroma1),tsi)

gqtest(mod_aroma1, fraction=1/3, order.by=model.frame(mod_aroma1)$aroma, alternative="two.sided")

#a variância parece diminuir no final do gráfico, formato de funil. 
# há dois pontos a serem investigados, abaixo de -2 (observações 20 e 30)

tsi <- rstudent(mod_aroma2);a <- max(tsi);b <- min(tsi)
plot(fitted(mod_aroma2),tsi,xlab="Valor Ajustado",ylab="Resíduo Studentizado", ylim=c(b-1,a+1), pch=16)
abline(h = c(-3, -2, 0, 2, 3),
       col = c("darkred", "red", "blue", "red", "darkred"),
       lty = c(3, 2, 2, 2, 3))
identify(fitted(mod_aroma2),tsi)

gqtest(mod_aroma2, fraction=1/3, order.by=model.frame(mod_aroma2)$aroma, alternative="two.sided")

tsi <- rstudent(mod_aroma3);a <- max(tsi);b <- min(tsi)
plot(fitted(mod_aroma3),tsi,xlab="Valor Ajustado",ylab="Resíduo Studentizado", ylim=c(b-1,a+1), pch=16)
abline(h = c(-3, -2, 0, 2, 3),
       col = c("darkred", "red", "blue", "red", "darkred"),
       lty = c(3, 2, 2, 2, 3))
identify(fitted(mod_aroma3),tsi)

gqtest(mod_aroma3, fraction=1/3, order.by=model.frame(mod_aroma3)$aroma, alternative="two.sided")

# a variância parece diminuir no final do gráfico, formato de funil. 
# há dois pontos a serem investigados, abaixo de -2 (observações 20 e 30)

tsi <- rstudent(mod_aroma4);a <- max(tsi);b <- min(tsi)
plot(fitted(mod_aroma4),tsi,xlab="Valor Ajustado",ylab="Resíduo Studentizado", ylim=c(b-1,a+1), pch=16)
abline(h = c(-3, -2, 0, 2, 3),
       col = c("darkred", "red", "blue", "red", "darkred"),
       lty = c(3, 2, 2, 2, 3))
identify(fitted(mod_aroma4),tsi)

gqtest(mod_aroma4, fraction=1/3, order.by=model.frame(mod_aroma4)$aroma, alternative="two.sided")


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
dwtest(mod_aroma1, alternative = "two.sided")

# observações 20 e 30 abaixo de -2

tsi <- rstudent(mod_aroma2);a <- max(tsi);b <- min(tsi)
plot(tsi, pch=16, xlab="Índice", ylab="Resíduo Studentizado",ylim=c(b-1,a+1))
abline(h = c(-3, -2, 0, 2, 3),
       col = c("darkred", "red", "blue", "red", "darkred"),
       lty = c(3, 2, 2, 2, 3))
identify(tsi)
   
dwtest(mod_aroma2, alternative = "two.sided")

tsi <- rstudent(mod_aroma3);a <- max(tsi);b <- min(tsi)
plot(tsi, pch=16, xlab="Índice", ylab="Resíduo Studentizado",ylim=c(b-1,a+1))
abline(h = c(-3, -2, 0, 2, 3),
       col = c("darkred", "red", "blue", "red", "darkred"),
       lty = c(3, 2, 2, 2, 3))
identify(tsi)

dwtest(mod_aroma3, alternative = "two.sided")

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
summary(mod_corpo) # 1,734, r2 = 0,3011

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

set.seed(123)

par(mfrow = c(1, 1))

envelope_LR(mod_corpo,  main.title = "") # poucos pontos pra fora (três ou quatro) 
qqPlot(mod_corpo) # um ponto pra fora

envelope_LR(mod_corpo1,  main.title = "") # sete/oito pontos pra fora e um no limite, ajuste ruim
qqPlot(mod_corpo1) # três pontos pra fora

envelope_LR(mod_corpo2,  main.title = "") # tres/ quatro pontos pra fora (bom)
qqPlot(mod_corpo2) # dois pontos pra fora

envelope_LR(mod_corpo3,  main.title = "") # cinco pontos pra fora e tres no limite
qqPlot(mod_corpo3) # três pontos pra fora

envelope_LR(mod_corpo4,  main.title = "") # quatro pontos pra fora e quatro no limite
qqPlot(mod_corpo4) # dois pontos pra fora

envelope_LR(mod_corpo5,  main.title = "") # dois/ três pra fora e três no limite
qqPlot(mod_corpo5) # dois pontos pra fora


# homocedasticidade

tsi <- rstudent(mod_corpo);a <- max(tsi);b <- min(tsi)
plot(fitted(mod_corpo),tsi,xlab="Valor Ajustado",ylab="Resíduo Studentizado", ylim=c(b-1,a+1), pch=16)
abline(h = c(-3, -2, 0, 2, 3),
       col = c("darkred", "red", "blue", "red", "darkred"),
       lty = c(3, 2, 2, 2, 3))
identify(fitted(mod_corpo),tsi)
# esquisito, investigar pontos 15 e 37
#20 é ponto aberrante
gqtest(mod_corpo, fraction=1/3, order.by=model.frame(mod_corpo)$corpo, alternative="two.sided")
# nçao rejeitou

tsi <- rstudent(mod_corpo1);a <- max(tsi);b <- min(tsi)
plot(fitted(mod_corpo1),tsi,xlab="Valor Ajustado",ylab="Resíduo Studentizado", ylim=c(b-1,a+1), pch=16)
abline(h = c(-3, -2, 0, 2, 3),
       col = c("darkred", "red", "blue", "red", "darkred"),
       lty = c(3, 2, 2, 2, 3))
identify(fitted(mod_corpo1),tsi)
# ok
# um ponto aberrante
gqtest(mod_corpo1, fraction=1/3, order.by=model.frame(mod_corpo1)$corpo, alternative="two.sided")

tsi <- rstudent(mod_corpo3);a <- max(tsi);b <- min(tsi)
plot(fitted(mod_corpo3),tsi,xlab="Valor Ajustado",ylab="Resíduo Studentizado", ylim=c(b-1,a+1), pch=16)
abline(h = c(-3, -2, 0, 2, 3),
       col = c("darkred", "red", "blue", "red", "darkred"),
       lty = c(3, 2, 2, 2, 3))
identify(fitted(mod_corpo3),tsi)

gqtest(mod_corpo3, fraction=1/3, order.by=model.frame(mod_corpo3)$corpo, alternative="two.sided")
# ok

tsi <- rstudent(mod_corpo4);a <- max(tsi);b <- min(tsi)
plot(fitted(mod_corpo4),tsi,xlab="Valor Ajustado",ylab="Resíduo Studentizado", ylim=c(b-1,a+1), pch=16)
abline(h = c(-3, -2, 0, 2, 3),
       col = c("darkred", "red", "blue", "red", "darkred"),
       lty = c(3, 2, 2, 2, 3))
identify(fitted(mod_corpo4),tsi)

gqtest(mod_corpo4, fraction=1/3, order.by=model.frame(mod_corpo4)$corpo, alternative="two.sided")
# ok

tsi <- rstudent(mod_corpo5);a <- max(tsi);b <- min(tsi)
plot(fitted(mod_corpo5),tsi,xlab="Valor Ajustado",ylab="Resíduo Studentizado", ylim=c(b-1,a+1), pch=16)
abline(h = c(-3, -2, 0, 2, 3),
       col = c("darkred", "red", "blue", "red", "darkred"),
       lty = c(3, 2, 2, 2, 3))
identify(fitted(mod_corpo5),tsi)

gqtest(mod_corpo5, fraction=1/3, order.by=model.frame(mod_corpo5)$corpo, alternative="two.sided")
# ok


## Não correlação dos erros
tsi <- rstudent(mod_corpo);a <- max(tsi);b <- min(tsi)
plot(tsi, pch=16, xlab="Índice", ylab="Resíduo Studentizado",ylim=c(b-1,a+1))
abline(h = c(-3, -2, 0, 2, 3),
       col = c("darkred", "red", "blue", "red", "darkred"),
       lty = c(3, 2, 2, 2, 3))
identify(tsi)

dwtest(mod_corpo, alternative = "two.sided") # ok

tsi <- rstudent(mod_corpo1);a <- max(tsi);b <- min(tsi)
plot(tsi, pch=16, xlab="Índice", ylab="Resíduo Studentizado",ylim=c(b-1,a+1))
abline(h = c(-3, -2, 0, 2, 3),
       col = c("darkred", "red", "blue", "red", "darkred"),
       lty = c(3, 2, 2, 2, 3))
identify(tsi)

dwtest(mod_corpo1, alternative = "two.sided") # ok

tsi <- rstudent(mod_corpo2);a <- max(tsi);b <- min(tsi)
plot(tsi, pch=16, xlab="Índice", ylab="Resíduo Studentizado",ylim=c(b-1,a+1))
abline(h = c(-3, -2, 0, 2, 3),
       col = c("darkred", "red", "blue", "red", "darkred"),
       lty = c(3, 2, 2, 2, 3))
identify(tsi)

dwtest(mod_corpo2, alternative = "two.sided")

tsi <- rstudent(mod_corpo3);a <- max(tsi);b <- min(tsi)
plot(tsi, pch=16, xlab="Índice", ylab="Resíduo Studentizado",ylim=c(b-1,a+1))
abline(h = c(-3, -2, 0, 2, 3),
       col = c("darkred", "red", "blue", "red", "darkred"),
       lty = c(3, 2, 2, 2, 3))
identify(tsi)

dwtest(mod_corpo3, alternative = "two.sided")

tsi <- rstudent(mod_corpo4);a <- max(tsi);b <- min(tsi)
plot(tsi, pch=16, xlab="Índice", ylab="Resíduo Studentizado",ylim=c(b-1,a+1))
abline(h = c(-3, -2, 0, 2, 3),
       col = c("darkred", "red", "blue", "red", "darkred"),
       lty = c(3, 2, 2, 2, 3))
identify(tsi)

dwtest(mod_corpo4, alternative = "two.sided")

tsi <- rstudent(mod_corpo5);a <- max(tsi);b <- min(tsi)
plot(tsi, pch=16, xlab="Índice", ylab="Resíduo Studentizado",ylim=c(b-1,a+1))
abline(h = c(-3, -2, 0, 2, 3),
       col = c("darkred", "red", "blue", "red", "darkred"),
       lty = c(3, 2, 2, 2, 3))
identify(tsi)

dwtest(mod_corpo5, alternative = "two.sided")


# ---------------------------------------------------------------------------------------------------------
mod_sabor <- lm(qualidade ~ sabor, data = wine)
summary(mod_sabor) # rse = 1.271 r2 = 0.62

mod_sabor1 <- lm(log(qualidade) ~ sabor, data = wine)
summary(mod_sabor1) #rse = 0.1094 r2 = 0.6062

mod_sabor2 <- lm(sqrt(qualidade) ~ sabor, data = wine)
summary(mod_sabor2) # rse = 1.292 r2 = 0.6116

mod_sabor3 <- lm(log(qualidade) ~ log(sabor), data = wine)
summary(mod_sabor3) # rse =0.109 r2 = 0.6089

mod_sabor4 <- lm(qualidade ~ log(sabor), data = wine)
summary(mod_sabor4) # rse = 0.1852 r2 = 0.6168 

par(mfrow = c(2, 3))

plot(sabor,qualidade, pch = 16) #
plot(sabor, log(qualidade), pch = 16)
plot(log(sabor), qualidade, pch = 16) #
plot(log(sabor), log(qualidade), pch = 16)
plot(sabor, sqrt(qualidade), pch = 16) #

envelope_LR(mod_sabor,  main.title = "") 
qqPlot(mod_sabor)

envelope_LR(mod_sabor1,  main.title = "") 
qqPlot(mod_sabor1)

envelope_LR(mod_sabor2,  main.title = "") 
qqPlot(mod_sabor2)

envelope_LR(mod_sabor3,  main.title = "") 
qqPlot(mod_sabor3)
shapiro.test(rstudent(mod_sabor3))

envelope_LR(mod_sabor4,  main.title = "") 
qqPlot(mod_sabor4)

tsi <- rstudent(mod_sabor);a <- max(tsi);b <- min(tsi)
plot(fitted(mod_sabor),tsi,xlab="Valor Ajustado",ylab="Resíduo Studentizado", ylim=c(b-1,a+1), pch=16)
abline(h = c(-3, -2, 0, 2, 3),
       col = c("darkred", "red", "blue", "red", "darkred"),
       lty = c(3, 2, 2, 2, 3))
identify(fitted(mod_sabor),tsi)

gqtest(mod_sabor, fraction=1/3, order.by=model.frame(mod_sabor)$sabor, alternative="two.sided")

tsi <- rstudent(mod_sabor1);a <- max(tsi);b <- min(tsi)
plot(fitted(mod_sabor1),tsi,xlab="Valor Ajustado",ylab="Resíduo Studentizado", ylim=c(b-1,a+1), pch=16)
abline(h = c(-3, -2, 0, 2, 3),
       col = c("darkred", "red", "blue", "red", "darkred"),
       lty = c(3, 2, 2, 2, 3))
identify(fitted(mod_sabor1),tsi)

gqtest(mod_sabor1, fraction=1/3, order.by=model.frame(mod_sabor1)$sabor, alternative="two.sided")

tsi <- rstudent(mod_sabor2);a <- max(tsi);b <- min(tsi)
plot(fitted(mod_sabor2),tsi,xlab="Valor Ajustado",ylab="Resíduo Studentizado", ylim=c(b-1,a+1), pch=16)
abline(h = c(-3, -2, 0, 2, 3),
       col = c("darkred", "red", "blue", "red", "darkred"),
       lty = c(3, 2, 2, 2, 3))
identify(fitted(mod_sabor2),tsi)

gqtest(mod_sabor2, fraction=1/3, order.by=model.frame(mod_sabor2)$sabor, alternative="two.sided")

tsi <- rstudent(mod_sabor3);a <- max(tsi);b <- min(tsi)
plot(fitted(mod_sabor3),tsi,xlab="Valor Ajustado",ylab="Resíduo Studentizado", ylim=c(b-1,a+1), pch=16)
abline(h = c(-3, -2, 0, 2, 3),
       col = c("darkred", "red", "blue", "red", "darkred"),
       lty = c(3, 2, 2, 2, 3))
identify(fitted(mod_sabor3),tsi)

gqtest(mod_sabor3, fraction=1/3, order.by=model.frame(mod_sabor3)$sabor, alternative="two.sided")

tsi <- rstudent(mod_sabor4);a <- max(tsi);b <- min(tsi)
plot(fitted(mod_sabor4),tsi,xlab="Valor Ajustado",ylab="Resíduo Studentizado", ylim=c(b-1,a+1), pch=16)
abline(h = c(-3, -2, 0, 2, 3),
       col = c("darkred", "red", "blue", "red", "darkred"),
       lty = c(3, 2, 2, 2, 3))
identify(fitted(mod_sabor4),tsi)

gqtest(mod_sabor4, fraction=1/3, order.by=model.frame(mod_sabor4)$sabor, alternative="two.sided")

tsi <- rstudent(mod_sabor);a <- max(tsi);b <- min(tsi)
plot(tsi, pch=16, xlab="Índice", ylab="Resíduo Studentizado",ylim=c(b-1,a+1))
abline(h = c(-3, -2, 0, 2, 3),
       col = c("darkred", "red", "blue", "red", "darkred"),
       lty = c(3, 2, 2, 2, 3))
identify(tsi)

dwtest(mod_sabor, alternative = "two.sided")

tsi <- rstudent(mod_sabor1);a <- max(tsi);b <- min(tsi)
plot(tsi, pch=16, xlab="Índice", ylab="Resíduo Studentizado",ylim=c(b-1,a+1))
abline(h = c(-3, -2, 0, 2, 3),
       col = c("darkred", "red", "blue", "red", "darkred"),
       lty = c(3, 2, 2, 2, 3))
identify(tsi)

dwtest(mod_sabor1, alternative = "two.sided")

tsi <- rstudent(mod_sabor2);a <- max(tsi);b <- min(tsi)
plot(tsi, pch=16, xlab="Índice", ylab="Resíduo Studentizado",ylim=c(b-1,a+1))
abline(h = c(-3, -2, 0, 2, 3),
       col = c("darkred", "red", "blue", "red", "darkred"),
       lty = c(3, 2, 2, 2, 3))
identify(tsi)

dwtest(mod_sabor2, alternative = "two.sided")

tsi <- rstudent(mod_sabor3);a <- max(tsi);b <- min(tsi)
plot(tsi, pch=16, xlab="Índice", ylab="Resíduo Studentizado",ylim=c(b-1,a+1))
abline(h = c(-3, -2, 0, 2, 3),
       col = c("darkred", "red", "blue", "red", "darkred"),
       lty = c(3, 2, 2, 2, 3))
identify(tsi)

dwtest(mod_sabor3, alternative = "two.sided")

tsi <- rstudent(mod_sabor4);a <- max(tsi);b <- min(tsi)
plot(tsi, pch=16, xlab="Índice", ylab="Resíduo Studentizado",ylim=c(b-1,a+1))
abline(h = c(-3, -2, 0, 2, 3),
       col = c("darkred", "red", "blue", "red", "darkred"),
       lty = c(3, 2, 2, 2, 3))
identify(tsi)

dwtest(mod_sabor4, alternative = "two.sided")

library(MASS)

boxlife <- boxcox(mod_sabor, plotit = TRUE)
lambda1 <- boxlife$x[which.max(boxlife$y)]
lambda1
wine$qualidade_boxcox <- ((wine$qualidade ^ lambda1) - 1) / lambda1

modelo_boxcox <- lm(qualidade_boxcox ~ sabor, data = wine)
summary(modelo_boxcox)

tsi <- rstudent(modelo_boxcox);a <- max(tsi);b <- min(tsi)
plot(tsi, pch=16, xlab="Índice", ylab="Resíduo Studentizado",ylim=c(b-1,a+1))
abline(h = c(-3, -2, 0, 2, 3),
       col = c("darkred", "red", "blue", "red", "darkred"),
       lty = c(3, 2, 2, 2, 3))
identify(tsi)

dwtest(modelo_boxcox, alternative = "two.sided")

boxlife <- boxcox(mod_aroma, plotit = TRUE)
lambda1 <- boxlife$x[which.max(boxlife$y)]
lambda1
wine$qualidade_boxcox <- ((wine$qualidade ^ lambda1) - 1) / lambda1

modelo_boxcox <- lm(qualidade_boxcox ~ aroma, data = wine)
summary(modelo_boxcox)

tsi <- rstudent(modelo_boxcox);a <- max(tsi);b <- min(tsi)
plot(tsi, pch=16, xlab="Índice", ylab="Resíduo Studentizado",ylim=c(b-1,a+1))
abline(h = c(-3, -2, 0, 2, 3),
       col = c("darkred", "red", "blue", "red", "darkred"),
       lty = c(3, 2, 2, 2, 3))
identify(tsi)

dwtest(modelo_boxcox, alternative = "two.sided")

envelope_LR(modelo_boxcox,  main.title = "") 
qqPlot(modelo_boxcox)

tsi <- rstudent(modelo_boxcox);a <- max(tsi);b <- min(tsi)
plot(fitted(modelo_boxcox),tsi,xlab="Valor Ajustado",ylab="Resíduo Studentizado", ylim=c(b-1,a+1), pch=16)
abline(h = c(-3, -2, 0, 2, 3),
       col = c("darkred", "red", "blue", "red", "darkred"),
       lty = c(3, 2, 2, 2, 3))
identify(fitted(mod_sabor4),tsi)

gqtest(modelo_boxcox, fraction=1/3, order.by=model.frame(modelo_boxcox)$aroma, alternative="two.sided")
