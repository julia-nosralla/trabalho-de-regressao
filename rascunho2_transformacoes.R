# transformações aroma

# original
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

# log na resposta
mod_aroma1 <- lm(log(qualidade) ~ aroma, data = wine)
summary(mod_aroma1)

ggplot(wine, aes(x = aroma, y = log(qualidade))) +
  geom_point(shape = 16, size = 2) +                     
  geom_smooth(method = "lm", se = FALSE, color = "#1F77B4") +
  labs(
    x = "Aroma",
    y = "log(Qualidade)"
  ) +
  meu_tema

# raiz quadrada na resposta
mod_aroma2 <- lm(sqrt(qualidade) ~ aroma, data = wine)
summary(mod_aroma2)

ggplot(wine, aes(x = aroma, y = sqrt(qualidade))) +
  geom_point(shape = 16, size = 2) +                     
  geom_smooth(method = "lm", se = FALSE, color = "#1F77B4") +
  labs(
    x = "Aroma",
    y = "sqrt(Qualidade)"
  ) +
  meu_tema

# log na reposta e na explicativa
mod_aroma3 <- lm(log(qualidade) ~ log(aroma), data = wine)
summary(mod_aroma3)

ggplot(wine, aes(x = log(aroma), y = log(qualidade))) +
  geom_point(shape = 16, size = 2) +                     
  geom_smooth(method = "lm", se = FALSE, color = "#1F77B4") +
  labs(
    x = "log(Aroma)",
    y = "log(Qualidade)"
  ) +
  meu_tema

boxlife <- boxcox(mod_aroma, plotit = TRUE)
lambda1 <- boxlife$x[which.max(boxlife$y)]
lambda1

# transformações claridade
mod_claridade <- lm(qualidade ~ claridade, data = wine)
summary(mod_claridade)

ggplot(wine, aes(x = claridade, y = qualidade)) +
  geom_point(shape = 16, size = 2) +                     
  geom_smooth(method = "lm", se = FALSE, color = "#1F77B4") +
  labs(
    x = "Claridade",
    y = "Qualidade"
  ) +
  meu_tema

# log na resposta
mod_claridade1 <- lm(log(qualidade) ~ claridade, data = wine)
summary(mod_claridade1)

ggplot(wine, aes(x = claridade, y = log(qualidade))) +
  geom_point(shape = 16, size = 2) +                     
  geom_smooth(method = "lm", se = FALSE, color = "#1F77B4") +
  labs(
    x = "Claridade",
    y = "log(Qualidade)"
  ) +
  meu_tema

# raiz quadrada na resposta
mod_claridade2 <- lm(sqrt(qualidade) ~ claridade, data = wine)
summary(mod_claridade2)

ggplot(wine, aes(x = claridade, y = sqrt(qualidade))) +
  geom_point(shape = 16, size = 2) +                     
  geom_smooth(method = "lm", se = FALSE, color = "#1F77B4") +
  labs(
    x = "Claridade",
    y = "sqrt(Qualidade)"
  ) +
  meu_tema

# log na reposta e na explicativa
mod_claridade3 <- lm(log(qualidade) ~ log(claridade), data = wine)
summary(mod_aroma3)

ggplot(wine, aes(x = log(claridade), y = log(qualidade))) +
  geom_point(shape = 16, size = 2) +                     
  geom_smooth(method = "lm", se = FALSE, color = "#1F77B4") +
  labs(
    x = "log(Aroma)",
    y = "log(Qualidade)"
  ) +
  meu_tema

# transformações corpo

# original

summary(mod_corpo)

ggplot(wine, aes(x = corpo, y = qualidade)) +
  geom_point(shape = 16, size = 2) +                     
  geom_smooth(method = "lm", se = FALSE, color = "#1F77B4") +
  labs(
    x = "Corpo",
    y = "Qualidade"
  ) +
  meu_tema

# log resposta

mod_corpo1 <- lm(log(qualidade) ~ corpo, data = wine)
summary(mod_corpo1)

ggplot(wine, aes(x = corpo, y = log(qualidade))) +
  geom_point(shape = 16, size = 2) +                     
  geom_smooth(method = "lm", se = FALSE, color = "#1F77B4") +
  labs(
    x = "Corpo",
    y = "log(Qualidade)"
  ) +
  meu_tema

# log explicativa

mod_corpo2 <- lm(qualidade ~ log(corpo), data = wine)
summary(mod_corpo2)

ggplot(wine, aes(x = log(corpo), y = qualidade)) +
  geom_point(shape = 16, size = 2) +                     
  geom_smooth(method = "lm", se = FALSE, color = "#1F77B4") +
  labs(
    x = "log(Corpo)",
    y = "Qualidade"
  ) +
  meu_tema
# ruim

# log explicativa e na resposta

mod_corpo3 <- lm(log(qualidade) ~ log(corpo), data = wine)
summary(mod_corpo3)

ggplot(wine, aes(x = log(corpo), y = log(qualidade))) +
  geom_point(shape = 16, size = 2) +                     
  geom_smooth(method = "lm", se = FALSE, color = "#1F77B4") +
  labs(
    x = "log(Corpo)",
    y = "log(Qualidade)"
  ) +
  meu_tema

# raiz quadrada na resposta

mod_corpo4 <- lm(sqrt(qualidade) ~ corpo, data = wine)
summary(mod_corpo4)

ggplot(wine, aes(x = corpo, y = sqrt(qualidade))) +
  geom_point(shape = 16, size = 2) +                     
  geom_smooth(method = "lm", se = FALSE, color = "#1F77B4") +
  labs(
    x = "Corpo",
    y = "sqrt(Qualidade)"
  ) +
  meu_tema

# ao quadrado na resposta

mod_corpo5 <- lm(qualidade^2 ~ corpo, data = wine)
summary(mod_corpo5)

ggplot(wine, aes(x = corpo, y = qualidade^2)) +
  geom_point(shape = 16, size = 2) +                     
  geom_smooth(method = "lm", se = FALSE, color = "#1F77B4") +
  labs(
    x = "Corpo",
    y = "Qualidade^2"
  ) +
  meu_tema

boxlife <- boxcox(mod_corpo, plotit = TRUE)
lambda1 <- boxlife$x[which.max(boxlife$y)]
lambda1

# transformações sabor

#original
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

#log resposta
mod_sabor1     <- lm(log(qualidade) ~ sabor, data = wine)
summary(mod_sabor1)

ggplot(wine, aes(x = sabor, y = log(qualidade))) +
  geom_point(shape = 16, size = 2) +                     
  geom_smooth(method = "lm", se = FALSE, color = "#1F77B4") +
  labs(
    x = "Sabor",
    y = "log(Qualidade)"
  ) +
  meu_tema

# log explicativa
mod_sabor2     <- lm(qualidade ~ log(sabor), data = wine)
summary(mod_sabor2)

ggplot(wine, aes(x = log(sabor), y = qualidade)) +
  geom_point(shape = 16, size = 2) +                     
  geom_smooth(method = "lm", se = FALSE, color = "#1F77B4") +
  labs(
    x = "log(Sabor)",
    y = "Qualidade"
  ) +
  meu_tema

# log explicativa e na resposta
mod_sabor3     <- lm(log(qualidade) ~ log(sabor), data = wine)
summary(mod_sabor3)

ggplot(wine, aes(x = log(sabor), y = log(qualidade))) +
  geom_point(shape = 16, size = 2) +                     
  geom_smooth(method = "lm", se = FALSE, color = "#1F77B4") +
  labs(
    x = "log(Sabor)",
    y = "log(Qualidade)"
  ) +
  meu_tema

# raiz quadrada resposta
mod_sabor4     <- lm(sqrt(qualidade) ~ sabor, data = wine)
summary(mod_sabor4)

ggplot(wine, aes(x = sabor, y = sqrt(qualidade))) +
  geom_point(shape = 16, size = 2) +                     
  geom_smooth(method = "lm", se = FALSE, color = "#1F77B4") +
  labs(
    x = "Sabor",
    y = "sqrt(Qualidade)"
  ) +
  meu_tema

# resposta a quadrado
mod_sabor5     <- lm(qualidade^2 ~ sabor, data = wine)
summary(mod_sabor5)

ggplot(wine, aes(x = sabor, y = qualidade^2)) +
  geom_point(shape = 16, size = 2) +                     
  geom_smooth(method = "lm", se = FALSE, color = "#1F77B4") +
  labs(
    x = "Sabor",
    y = "Qualidade^2"
  ) +
  meu_tema

tsi <- rstudent(mod_sabor3);a <- max(tsi);b <- min(tsi)
plot(tsi, pch=16, xlab="Índice", ylab="Resíduo Studentizado",ylim=c(b-1,a+1))
abline(h = c(-3, -2, 0, 2, 3),
       col = c("darkred", "red", "blue", "red", "darkred"),
       lty = c(3, 2, 2, 2, 3))
# Teste de não correlação    
dwtest(mod_aroma, alternative = "two.sided")

boxlife <- boxcox(mod_sabor, plotit = TRUE)
lambda1 <- boxlife$x[which.max(boxlife$y)]
lambda1
