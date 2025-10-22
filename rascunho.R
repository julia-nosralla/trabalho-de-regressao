mod_aroma <- lm(qualidade ~ aroma, data = wine)
summary(mod_aroma)

ggplot(wine, aes(x = aroma, y = qualidade)) +
  geom_point(shape = 16, size = 2) +                     
  geom_smooth(method = "lm", se = FALSE, color = "#1F77B4") +
  labs(
    x = "Aroma",
    y = "Qualidade"
  ) +
  meu_tema

# suposições

## Normalidade 
envelope_LR(mod_aroma,  main.title = "")


# Teste de Normalidade
shapiro.test(rstudent(mod_aroma))

## Homogeneidade
tsi <- rstudent(mod_aroma);a <- max(tsi);b <- min(tsi)

plot(fitted(mod_aroma),tsi,xlab="Valor Ajustado",ylab="Resíduo Studentizado", ylim=c(b-1,a+1), pch=16)
abline(h = c(-3, -2, 0, 2, 3),
       col = c("darkred", "red", "blue", "red", "darkred"),
       lty = c(3, 2, 2, 2, 3))
# Teste de homocedasticidade 
gqtest(mod_aroma, fraction=1/3, order.by=model.frame(mod_aroma)$aroma, alternative="two.sided")

## Não correlação dos erros
tsi <- rstudent(mod_aroma);a <- max(tsi);b <- min(tsi)
plot(tsi, pch=16, xlab="Índice", ylab="Resíduo Studentizado",ylim=c(b-1,a+1))
abline(h = c(-3, -2, 0, 2, 3),
       col = c("darkred", "red", "blue", "red", "darkred"),
       lty = c(3, 2, 2, 2, 3))
# Teste de não correlação    
dwtest(mod_aroma, alternative = "two.sided")

# há autocorrelação

# -------------------------------------------------------------------------------------------

mod_corpo <- lm(qualidade ~ corpo, data = wine)
summary(mod_corpo)

ggplot(wine, aes(x = corpo, y = qualidade)) +
  geom_point(shape = 16, size = 2) +                     
  geom_smooth(method = "lm", se = FALSE, color = "#1F77B4") +
  labs(
    x = "Corpo",
    y = "Qualidade"
  ) +
  meu_tema

# suposições

## Normalidade 
envelope_LR(mod_corpo,  main.title = "")
qqPlot(mod_corpo)
# Teste de Normalidade
shapiro.test(rstudent(mod_corpo))

## Homogeneidade
tsi <- rstudent(mod_corpo);a <- max(tsi);b <- min(tsi)

plot(fitted(mod_corpo),tsi,xlab="Valor Ajustado",ylab="Resíduo Studentizado", ylim=c(b-1,a+1), pch=16)
abline(h = c(-3, -2, 0, 2, 3),
       col = c("darkred", "red", "blue", "red", "darkred"),
       lty = c(3, 2, 2, 2, 3))
# Teste de homocedasticidade 
gqtest(mod_corpo, fraction=1/3, order.by=model.frame(mod_corpo)$corpo, alternative="two.sided")

## Não correlação dos erros
tsi <- rstudent(mod_corpo);a <- max(tsi);b <- min(tsi)
plot(tsi, pch=16, xlab="Índice", ylab="Resíduo Studentizado",ylim=c(b-1,a+1))
abline(h = c(-3, -2, 0, 2, 3),
       col = c("darkred", "red", "blue", "red", "darkred"),
       lty = c(3, 2, 2, 2, 3))
# Teste de não correlação    
dwtest(mod_corpo, alternative = "two.sided")

# -------------------------------------------------------------------------------------------
mod_sabor     <- lm(qualidade ~ sabor, data = wine)
summary(mod_sabor)

ggplot(wine, aes(x = sabor, y = qualidade)) +
  geom_point(shape = 16, size = 2) +                     
  geom_smooth(method = "lm", se = FALSE, color = "#1F77B4") +
  labs(
    x = "Sabor",
    y = "Qualidade"
  ) +
  meu_tema

# suposições

## Normalidade 
envelope_LR(mod_sabor,  main.title = "")
# Teste de Normalidade
shapiro.test(rstudent(mod_sabor))

## Homogeneidade
tsi <- rstudent(mod_sabor);a <- max(tsi);b <- min(tsi)

plot(fitted(mod_sabor),tsi,xlab="Valor Ajustado",ylab="Resíduo Studentizado", ylim=c(b-1,a+1), pch=16)
abline(h = c(-3, -2, 0, 2, 3),
       col = c("darkred", "red", "blue", "red", "darkred"),
       lty = c(3, 2, 2, 2, 3))
# Teste de homocedasticidade 
gqtest(mod_sabor, fraction=1/3, order.by=model.frame(mod_sabor)$sabor, alternative="two.sided")

## Não correlação dos erros
tsi <- rstudent(mod_sabor);a <- max(tsi);b <- min(tsi)
plot(tsi, pch=16, xlab="Índice", ylab="Resíduo Studentizado",ylim=c(b-1,a+1))
abline(h = c(-3, -2, 0, 2, 3),
       col = c("darkred", "red", "blue", "red", "darkred"),
       lty = c(3, 2, 2, 2, 3))
# Teste de não correlação    
dwtest(mod_sabor, alternative = "two.sided")

# há autocorelção entre os resíduos
