library(readr)
library(tidyverse)
require(car)
require(MASS)
require(lmtest)
#install.packages("glmtoolbox")
require(glmtoolbox)

wine <- read.table("wine.txt", header=TRUE)
attach(wine)

source("envelope.R")

# Nosso interesse aqui é explicar a "qualidade" do vinho em função de uma única 
# variável escolhida a partir demais variáveis disponíveis. As variáveis que podem ser 
# utilizadas individualmente para explicar a qualidade do vinho são:


# Variáveis -----------------------------------------------------------------------------------------------------

# claridade;
# aroma;
# corpo; 
# sabor;
# aromac: aroma do tonel de carvalho.

#*********************************************** CÓDIGOS *********************************************************#
## Padronização -------------------------------------------------------------------------------------------------
estat_colors = c(
  "#1F77B4", "#003366", "#CC9900",
  "#663333", "#FF6600", "#CC9966",
  "#999966", "#006606", "#008091", 
  "#041835", "#666666" )

theme_estat = function(...) {
  theme = ggplot2::theme_bw() +
    ggplot2::theme(
      axis.title.y = ggplot2::element_text(colour = "black", size = 12),
      axis.title.x = ggplot2::element_text(colour = "black", size = 12),
      axis.text = ggplot2::element_text(colour = "black", size = 9.5),
      panel.border = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(colour = "black"),
      legend.position = "top",
      ...
    )
  
  return(
    list(
      theme,
      scale_fill_manual(values = estat_colors),
      scale_colour_manual(values = estat_colors)
    )
  )
}

meu_tema = theme_minimal() +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    plot.title = element_text(hjust = 0.5, size = 16)
  )

## Análise Descritiva das Variáveis ----------------------------------------------------------------------------

# qualidade --------------------------------------------------------------------
summary(wine$qualidade)
cat("Desvio padrão:", sd(wine$qualidade, na.rm = TRUE), "\n")
cat("Valores ausentes:", sum(is.na(wine$qualidade)), "\n")

# Histograma
ggplot(wine) +
  aes(x = qualidade) +
  geom_histogram(colour = "white", fill = "#51A5C5", binwidth = 1) +
  labs(
    x = "Qualidade", 
    y = "Frequência Absoluta",
    title = "Histograma da variável qualidade"
  ) +
  theme_estat()+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(wine) +
  aes(x = factor(""), y = qualidade) +
  geom_boxplot(fill = "#51A5C5", width = 0.3) + 
  guides(fill = FALSE) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "white") +
  labs(
    x = "", 
    y = "Qualidade"
  ) +
  theme_estat() +
  theme(plot.title = element_text(hjust = 0.5))

ggsave("graficos/box_qualidade.pdf", width = 158, height = 93, units = "mm")

# claridade --------------------------------------------------------------------
    summary(wine$claridade)
    cat("Desvio padrão:", sd(wine$claridade, na.rm = TRUE), "\n")
    cat("Valores ausentes:", sum(is.na(wine$claridade)), "\n")
    
    # Histograma
    ggplot(wine) +
      aes(x = claridade) +
      geom_histogram(colour = "white", fill = "#51A5C5") +
      labs(
        x = "Claridade", 
        y = "Frequência Absoluta",
        title = "Histograma da variável claridade"
      ) +
      theme_estat()+
      theme(plot.title = element_text(hjust = 0.5))
    
    # Boxplot
    ggplot(wine) +
      aes(x = factor(""), y = claridade) +
      geom_boxplot(fill = "#51A5C5", width = 0.3) + 
      guides(fill = FALSE) +
      stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "white") +
      labs(
        x = "", 
        y = "Claridade"
      ) +
      theme_estat() +
      theme(plot.title = element_text(hjust = 0.5))
    
    ggsave("graficos/box_claridade.pdf", width = 158, height = 93, units = "mm")
    
    #dipersao
    ggplot(wine, aes(x = claridade, y = qualidade)) +
      geom_point(shape = 16, size = 2) +                     
      geom_smooth(method = "lm", se = FALSE, color = "#51A5C5") +
      labs(
        x = "Claridade",
        y = "Qualidade"
      ) +
      meu_tema
    
    # não mostra correlação linear entre qualidade e claridade
    
    ggsave("graficos/disp_claridade.pdf", width = 158, height = 93, units = "mm")


# aroma ------------------------------------------------------------------------
    summary(wine$aroma)
    cat("Desvio padrão:", sd(wine$aroma, na.rm = TRUE), "\n")
    cat("Valores ausentes:", sum(is.na(wine$aroma)), "\n")
    
    # Histograma
    ggplot(wine) +
      aes(x = aroma) +
      geom_histogram(colour = "white", fill = "#51A5C5", binwidth = 1) +
      labs(
        x = "Aroma", 
        y = "Frequência Absoluta",
        title = "Histograma da variável aroma"
      ) +
      theme_estat()+
      theme(plot.title = element_text(hjust = 0.5))
    
    # Boxplot
    ggplot(wine) +
      aes(x = factor(""), y = aroma) +
      geom_boxplot(fill = "#51A5C5", width = 0.3) + 
      guides(fill = FALSE) +
      stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "white") +
      labs(
        x = "", 
        y = "Aroma"
      ) +
      theme_estat() +
      theme(plot.title = element_text(hjust = 0.5))
    
    ggsave("graficos/box_aroma.pdf", width = 158, height = 93, units = "mm")
    
    ggplot(wine, aes(x = aroma, y = qualidade)) +
      geom_point(shape = 16, size = 2) +                     
      geom_smooth(method = "lm", se = FALSE, color = "#51A5C5") +
      labs(
        x = "Aroma",
        y = "Qualidade"
      ) +
      meu_tema
    
    ggsave("graficos/disp_aroma.pdf", width = 158, height = 93, units = "mm")



# corpo ------------------------------------------------------------------------
    summary(wine$corpo)
    cat("Desvio padrão:", sd(wine$corpo, na.rm = TRUE), "\n")
    cat("Valores ausentes:", sum(is.na(wine$corpo)), "\n")
    
    # Histograma
    ggplot(wine) +
      aes(x = corpo) +
      geom_histogram(colour = "white", fill = "#51A5C5", binwidth = 1) +
      labs(
        x = "Corpo", 
        y = "Frequência Absoluta",
        title = "Histograma da variável corpo"
      ) +
      theme_estat()+
      theme(plot.title = element_text(hjust = 0.5))
    
    # Boxplot
    ggplot(wine) +
      aes(x = factor(""), y = corpo) +
      geom_boxplot(fill = "#51A5C5", width = 0.3) + 
      guides(fill = FALSE) +
      stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "white") +
      labs(
        x = "", 
        y = "Corpo"
      ) +
      theme_estat() +
      theme(plot.title = element_text(hjust = 0.5))
    
    ggsave("graficos/box_corpo.pdf", width = 158, height = 93, units = "mm")
    
    ggplot(wine, aes(x = corpo, y = qualidade)) +
      geom_point(shape = 16, size = 2) +                     
      geom_smooth(method = "lm", se = FALSE, color = "#51A5C5") +
      labs(
        x = "Corpo",
        y = "Qualidade"
      ) +
      meu_tema
    
    ggsave("graficos/disp_corpo.pdf", width = 158, height = 93, units = "mm")


# sabor ------------------------------------------------------------------------
    summary(wine$sabor)
    cat("Desvio padrão:", sd(wine$sabor, na.rm = TRUE), "\n")
    cat("Valores ausentes:", sum(is.na(wine$sabor)), "\n")
    
    # Histograma
    ggplot(wine) +
      aes(x = sabor) +
      geom_histogram(colour = "white", fill = "#51A5C5", binwidth = 1) +
      labs(
        x = "Sabor", 
        y = "Frequência Absoluta",
        title = "Histograma da variável sabor"
      ) +
      theme_estat()+
      theme(plot.title = element_text(hjust = 0.5))
    
    # Boxplot
    ggplot(wine) +
      aes(x = factor(""), y = sabor) +
      geom_boxplot(fill = "#51A5C5", width = 0.3) + 
      guides(fill = FALSE) +
      stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "white") +
      labs(
        x = "", 
        y = "Sabor"
      ) +
      theme_estat() +
      theme(plot.title = element_text(hjust = 0.5))
    
    ggsave("graficos/box_sabor.pdf", width = 158, height = 93, units = "mm")
    
    ggplot(wine, aes(x = sabor, y = qualidade)) +
      geom_point(shape = 16, size = 2) +                     
      geom_smooth(method = "lm", se = FALSE, color = "#51A5C5") +
      labs(
        x = "Sabor",
        y = "Qualidade"
      ) +
      meu_tema
    
    ggsave("graficos/disp_sabor.pdf", width = 158, height = 93, units = "mm")

# aromac ------------------------------------------------------------------------
    summary(wine$aromac)
    cat("Desvio padrão:", sd(wine$aromac, na.rm = TRUE), "\n")
    cat("Valores ausentes:", sum(is.na(wine$aromac)), "\n")
    
    # Histograma
    ggplot(wine) +
      aes(x = aromac) +
      geom_histogram(colour = "white", fill = "#51A5C5", binwidth = 1) +
      labs(
        x = "Aroma do Tonel de Carvalho", 
        y = "Frequência Absoluta",
        title = "Histograma da variável aroma"
      ) +
      theme_estat()+
      theme(plot.title = element_text(hjust = 0.5))
    
    # Boxplot
    ggplot(wine) +
      aes(x = factor(""), y = aromac) +
      geom_boxplot(fill = "#51A5C5", width = 0.3) + 
      guides(fill = FALSE) +
      stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "white") +
      labs(
        x = "", 
        y = "Aroma"
      ) +
      theme_estat() +
      theme(plot.title = element_text(hjust = 0.5))
    
    ggsave("graficos/box_aromac.pdf", width = 158, height = 93, units = "mm")
    
    ggplot(wine, aes(x = aromac, y = qualidade)) +
      geom_point(shape = 16, size = 2) +                     
      geom_smooth(method = "lm", se = FALSE, color = "#51A5C5") +
      labs(
        x = "Aroma do Tonel de Carvalho",
        y = "Qualidade"
      ) +
      meu_tema
    
    # não apresenta correlação linear
    
    ggsave("graficos/disp_aromac.pdf", width = 158, height = 93, units = "mm")
    
    

## Gráficos de dispersao da variável de interesse com cada covariável -------------------------------------------
    
    pairs(wine, main = "Matriz de Dispersão")
    
# Calculando os testes para testar as suposições ------------------------------------------------------------------
    
#* Normalidade
#* Homocedasticidade (Homogeneidade)
#* Não correlação dos erros
    
  source("envelope.R") #qqplot normal com envelope simulado de 90%
    
# Homogeneidade 
    # tsi <- rstudent(ajuste1);a <- max(tsi);b <- min(tsi)
    # plot(fitted(ajuste1),tsi,xlab="Valor Ajustado",ylab="Resíduo Studentizado", ylim=c(b-1,a+1), pch=16)
    # abline(-2,0,lty=2, col="red", lwd=2)
    # abline(2,0,lty=2, col="red", lwd=2)
    # abline(0,0,lty=2, col="blue",lwd=2)
    # gqtest(ajuste6, fraction=1/3, order.by=model.frame(ajuste6)$HEALTHEXPEND, alternative="two.sided")
    # p-valor < 0.05: Há heterocedasticidade, e a suposição de variância constante dos resíduos é violada.
    # p-valor > 0.05: Não há evidências de heterocedasticidade, ou seja, a variância dos resíduos é constante e o modelo é adequado.
    
# Normalidade 
    # envelope_LR(ajuste1,  main.title = "Envelope")
    # shapiro.test(rstudent(fit1))
    # quanto mais proximo o W do Shapiro, mais proximo da Normal
    
# Não correlação dos erros
    # tsi <- rstudent(fit11);a <- max(tsi);b <- min(tsi)
    # plot(tsi, pch=16, xlab="índice", ylab="Resíduo Studentizado",ylim=c(b-1,a+1))
    # abline(-2,0,lty=2, col="red", lwd=2)
    # abline(2,0,lty=2, col="red", lwd=2)
    # abline(0,0,lty=2, col="blue",lwd=2)
    # Quanto mais próximo o DW de 2, mais não correlacionado são os erros
    
    
# Calculando os testes para os modelos
    
# Ajuste modelo claridade ----
  ## Normalidade 
  envelope_LR(mod_claridade,  main.title = "")
  # Teste de Normalidade
  shapiro.test(rstudent(mod_claridade))
    
  ## Homogeneidade
  tsi <- rstudent(mod_claridade);a <- max(tsi);b <- min(tsi)
    
  plot(fitted(mod_claridade),tsi,xlab="Valor Ajustado",ylab="Resíduo Studentizado", ylim=c(b-1,a+1), pch=16)
  abline(-2,0,lty=2, col="red", lwd=2)
  abline(2,0,lty=2, col="red", lwd=2)
  abline(0,0,lty=2, col="blue",lwd=2)
  # Teste de homocedasticidade 
  gqtest(mod_claridade, fraction=1/3, order.by=model.frame(mod_claridade)$claridade, alternative="two.sided")
    
  ## Não correlação dos erros
  tsi <- rstudent(mod_claridade);a <- max(tsi);b <- min(tsi)
  plot(tsi, pch=16, xlab="Índice", ylab="Resíduo Studentizado",ylim=c(b-1,a+1))
  abline(-2,0,lty=2, col="red", lwd=2)
  abline(2,0,lty=2, col="red", lwd=2)
  abline(0,0,lty=2, col="blue",lwd=2)
  # Teste de não correlação    
  dwtest(mod_claridade, alternative = "two.sided")
  
# Ajuste modelo aroma --------------------------------------------------------
  par(mfrow = c(1, 2))  
  
 mod_aroma <- lm(qualidade ~ aroma, data = wine); summary(mod_aroma)
  
  ggplot(wine, aes(x = aroma, y = qualidade)) +
    geom_point(shape = 16, size = 2) +                     
    geom_smooth(method = "lm", se = FALSE, color = "#1F77B4") +
    labs(
      x = "Aroma",
      y = "Qualidade"
    ) +
    meu_tema
  
  # log na resposta

  mod_aroma1 <- lm(log(qualidade) ~ aroma, data = wine); summary(mod_aroma1)
  
  ggplot(wine, aes(x = aroma, y = log(qualidade))) +
    geom_point(shape = 16, size = 2) +                     
    geom_smooth(method = "lm", se = FALSE, color = "#1F77B4") +
    labs(
      x = "Aroma",
      y = "Log(qualidade)"
    ) +
    meu_tema
  
  ggsave("graficos/disp_aroma_log.pdf", width = 158, height = 93, units = "mm")
  
  # Ajuste modelo corpo --------------------------------------------------------
  par(mfrow = c(1, 2))  
  
  mod_corpo<- lm(qualidade ~ corpo, data = wine); summary(mod_corpo)
  
  ggplot(wine, aes(x = corpo, y = qualidade)) +
    geom_point(shape = 16, size = 2) +                     
    geom_smooth(method = "lm", se = FALSE, color = "#1F77B4") +
    labs(
      x = "Corpo",
      y = "Qualidade"
    ) +
    meu_tema
  
# Ajuste mdelo sabor -----------------------------------------------------------
 
  # original
  
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
  
  # log na resposta
  
  mod_sabor1 <- lm(log(qualidade) ~ sabor, data = wine)
  summary(mod_sabor1)
  
  ggplot(wine, aes(x = sabor, y = log(qualidade))) +
    geom_point(shape = 16, size = 2) +                     
    geom_smooth(method = "lm", se = FALSE, color = "#1F77B4") +
    labs(
      x = "Sabor",
      y = "Log(qualidade)"
    ) +
    meu_tema
  
  # raiz quadrada resposta
  mod_sabor2     <- lm(sqrt(qualidade) ~ sabor, data = wine)
  summary(mod_sabor2)
  
  ggplot(wine, aes(x = sabor, y = sqrt(qualidade))) +
    geom_point(shape = 16, size = 2) +                     
    geom_smooth(method = "lm", se = FALSE, color = "#1F77B4") +
    labs(
      x = "Sabor",
      y = "sqrt(Qualidade)"
    ) +
    meu_tema
  
    
######## Normalidade ###############################################################
  
  # aroma
  envelope_LR(mod_aroma, main.title = "")
  
  envelope_LR(mod_aroma1, main.title = "")
  
  envelope_LR(mod_aroma2, main.title = "")
  
  qqPlot(mod_aroma,
         xlab = "Quantis teóricos",
         ylab = "Resíduos studentizados")
  
  qqPlot(mod_aroma1,
         xlab = "Quantis teóricos",
         ylab = "Resíduos studentizados")
  
  qqPlot(mod_aroma2,
         xlab = "Quantis teóricos",
         ylab = "Resíduos studentizados")

  # Teste de Normalidade
  shapiro.test(rstudent(mod_aroma))
  shapiro.test(rstudent(mod_aroma1))
  shapiro.test(rstudent(mod_aroma2))
  
  # corpo
  envelope_LR(mod_corpo, main.title = "")
  
  qqPlot(mod_corpo,
         xlab = "Quantis teóricos",
         ylab = "Resíduos studentizados")
  
  # Teste de Normalidade
  shapiro.test(rstudent(mod_corpo))
  
  # sabor
  envelope_LR(mod_sabor, main.title = "")
  
  envelope_LR(mod_sabor1, main.title = "")
  
  envelope_LR(mod_sabor2, main.title = "")
  
  envelope_LR(mod_sabor3)
  
  qqPlot(mod_sabor,
         xlab = "Quantis teóricos",
         ylab = "Resíduos studentizados")
  
  qqPlot(mod_sabor1,
         xlab = "Quantis teóricos",
         ylab = "Resíduos studentizados")
  
  qqPlot(mod_sabor2,
         xlab = "Quantis teóricos",
         ylab = "Resíduos studentizados")
  
  qqPlot(mod_sabor3,
         xlab = "Quantis teóricos",
         ylab = "Resíduos studentizados")
  
  # Teste de Normalidade
  shapiro.test(rstudent(mod_sabor))
  shapiro.test(rstudent(mod_sabor1))
  shapiro.test(rstudent(mod_sabor2))
  
  
  ## Homogeneidade
  tsi <- rstudent(mod_claridade);a <- max(tsi);b <- min(tsi)
  
  plot(fitted(mod_claridade),tsi,xlab="Valor Ajustado",ylab="Resíduo Studentizado", ylim=c(b-1,a+1), pch=16)
  abline(-2,0,lty=2, col="red", lwd=2)
  abline(2,0,lty=2, col="red", lwd=2)
  abline(0,0,lty=2, col="blue",lwd=2)
  # Teste de homocedasticidade 
  gqtest(mod_claridade, fraction=1/3, order.by=model.frame(mod_claridade)$claridade, alternative="two.sided")
  
  ## Não correlação dos erros
  tsi <- rstudent(mod_claridade);a <- max(tsi);b <- min(tsi)
  plot(tsi, pch=16, xlab="Índice", ylab="Resíduo Studentizado",ylim=c(b-1,a+1))
  abline(-2,0,lty=2, col="red", lwd=2)
  abline(2,0,lty=2, col="red", lwd=2)
  abline(0,0,lty=2, col="blue",lwd=2)
  # Teste de não correlação    
  dwtest(mod_claridade, alternative = "two.sided")
  
  # aroma
  
  tsi <- rstudent(mod_aroma);a <- max(tsi);b <- min(tsi)
  
  plot(fitted(mod_aroma),tsi,xlab="Valor Ajustado",ylab="Resíduo Studentizado", ylim=c(b-1,a+1), pch=16)
  abline(-2,0,lty=2, col="red", lwd=2)
  abline(2,0,lty=2, col="red", lwd=2)
  abline(0,0,lty=2, col="blue",lwd=2)
  identify(fitted(mod_aroma),tsi) # 20, 30
  # Teste de homocedasticidade 
  gqtest(mod_aroma, fraction=1/3, order.by=model.frame(mod_aroma)$aroma, alternative="two.sided")
  
  tsi <- rstudent(mod_aroma1);a <- max(tsi);b <- min(tsi)
  plot(fitted(mod_aroma1),tsi,xlab="Valor Ajustado",ylab="Resíduo Studentizado", ylim=c(b-1,a+1), pch=16)
  abline(-2,0,lty=2, col="red", lwd=2)
  abline(2,0,lty=2, col="red", lwd=2)
  abline(0,0,lty=2, col="blue",lwd=2)
  identify(fitted(mod_aroma1),tsi) # 20, 30
  # Teste de homocedasticidade 
  gqtest(mod_aroma1, fraction=1/3, order.by=model.frame(mod_aroma1)$aroma, alternative="two.sided")
  
  tsi <- rstudent(mod_aroma2);a <- max(tsi);b <- min(tsi)
  plot(fitted(mod_aroma2),tsi,xlab="Valor Ajustado",ylab="Resíduo Studentizado", ylim=c(b-1,a+1), pch=16)
  abline(-2,0,lty=2, col="red", lwd=2)
  abline(2,0,lty=2, col="red", lwd=2)
  abline(0,0,lty=2, col="blue",lwd=2)
  identify(fitted(mod_aroma1),tsi) # 20, 30
  # Teste de homocedasticidade 
  gqtest(mod_aroma2, fraction=1/3, order.by=model.frame(mod_aroma2)$aroma, alternative="two.sided")
  
  # corpo
  
  tsi <- rstudent(mod_corpo);a <- max(tsi);b <- min(tsi)
  
  plot(fitted(mod_corpo),tsi,xlab="Valor Ajustado",ylab="Resíduo Studentizado", ylim=c(b-1,a+1), pch=16)
  abline(-2,0,lty=2, col="red", lwd=2)
  abline(2,0,lty=2, col="red", lwd=2)
  abline(0,0,lty=2, col="blue",lwd=2)
  identify(fitted(mod_corpo),tsi) # 15, 20
  # Teste de homocedasticidade 
  gqtest(mod_corpo, fraction=1/3, order.by=model.frame(mod_corpo)$corpo, alternative="two.sided")
  
  # sabor
  
  tsi <- rstudent(mod_sabor);a <- max(tsi);b <- min(tsi)
  plot(fitted(mod_sabor),tsi,xlab="Valor Ajustado",ylab="Resíduo Studentizado", ylim=c(b-1,a+1), pch=16)
  abline(-2,0,lty=2, col="red", lwd=2)
  abline(2,0,lty=2, col="red", lwd=2)
  abline(0,0,lty=2, col="blue",lwd=2)
  identify(fitted(mod_sabor),tsi) # 9, 20
  # Teste de homocedasticidade 
  gqtest(mod_sabor, fraction=1/3, order.by=model.frame(mod_sabor)$sabor, alternative="two.sided")
  
  
  tsi <- rstudent(mod_sabor1);a <- max(tsi);b <- min(tsi)
  plot(fitted(mod_sabor1),tsi,xlab="Valor Ajustado",ylab="Resíduo Studentizado", ylim=c(b-1,a+1), pch=16)
  abline(-2,0,lty=2, col="red", lwd=2)
  abline(2,0,lty=2, col="red", lwd=2)
  abline(0,0,lty=2, col="blue",lwd=2)
  # Teste de homocedasticidade 
  gqtest(mod_sabor1, fraction=1/3, order.by=model.frame(mod_sabor1)$sabor, alternative="two.sided")
  
  
  tsi <- rstudent(mod_sabor2);a <- max(tsi);b <- min(tsi)
  plot(fitted(mod_sabor2),tsi,xlab="Valor Ajustado",ylab="Resíduo Studentizado", ylim=c(b-1,a+1), pch=16)
  abline(-2,0,lty=2, col="red", lwd=2)
  abline(2,0,lty=2, col="red", lwd=2)
  abline(0,0,lty=2, col="blue",lwd=2)
  # Teste de homocedasticidade 
  gqtest(mod_sabor2, fraction=1/3, order.by=model.frame(mod_sabor2)$sabor, alternative="two.sided")
  
  tsi <- rstudent(mod_sabor3);a <- max(tsi);b <- min(tsi)
  plot(fitted(mod_sabor3),tsi,xlab="Valor Ajustado",ylab="Resíduo Studentizado", ylim=c(b-1,a+1), pch=16)
  abline(-2,0,lty=2, col="red", lwd=2)
  abline(2,0,lty=2, col="red", lwd=2)
  abline(0,0,lty=2, col="blue",lwd=2)
  # Teste de homocedasticidade 
  gqtest(mod_sabor3, fraction=1/3, order.by=model.frame(mod_sabor3)$sabor, alternative="two.sided")
  
  bptest(mod_aroma)
  bptest(mod_aroma1)
  bptest(mod_corpo)
  bptest(mod_sabor)
  bptest(mod_sabor1)
  bptest(mod_sabor2)
  
  ## Não correlação dos erros
  tsi <- rstudent(mod_sabor);a <- max(tsi);b <- min(tsi)
  plot(tsi, pch=16, xlab="Índice", ylab="Resíduo Studentizado",ylim=c(b-1,a+1))
  abline(-2,0,lty=2, col="red", lwd=2)
  abline(2,0,lty=2, col="red", lwd=2)
  abline(0,0,lty=2, col="blue",lwd=2)
  # Teste de não correlação    
  dwtest(mod_sabor, alternative = "two.sided")
  
  # aroma
  
  tsi <- rstudent(mod_aroma);a <- max(tsi);b <- min(tsi)
  plot(tsi, pch=16, xlab="Índice", ylab="Resíduo Studentizado",ylim=c(b-1,a+1))
  abline(-2,0,lty=2, col="red", lwd=2)
  abline(2,0,lty=2, col="red", lwd=2)
  abline(0,0,lty=2, col="blue",lwd=2)
  # Teste de não correlação    
  dwtest(mod_aroma, alternative = "two.sided")

  tsi <- rstudent(mod_aroma1);a <- max(tsi);b <- min(tsi)
  plot(tsi, pch=16, xlab="Índice", ylab="Resíduo Studentizado",ylim=c(b-1,a+1))
  abline(-2,0,lty=2, col="red", lwd=2)
  abline(2,0,lty=2, col="red", lwd=2)
  abline(0,0,lty=2, col="blue",lwd=2)
  # Teste de não correlação    
  dwtest(mod_aroma1, alternative = "two.sided")  
  
  tsi <- rstudent(mod_aroma2);a <- max(tsi);b <- min(tsi)
  plot(tsi, pch=16, xlab="Índice", ylab="Resíduo Studentizado",ylim=c(b-1,a+1))
  abline(-2,0,lty=2, col="red", lwd=2)
  abline(2,0,lty=2, col="red", lwd=2)
  abline(0,0,lty=2, col="blue",lwd=2)
  # Teste de não correlação    
  dwtest(mod_aroma2, alternative = "two.sided") 

  # corpo
  
  tsi <- rstudent(mod_corpo);a <- max(tsi);b <- min(tsi)
  plot(tsi, pch=16, xlab="Índice", ylab="Resíduo Studentizado",ylim=c(b-1,a+1))
  abline(-2,0,lty=2, col="red", lwd=2)
  abline(2,0,lty=2, col="red", lwd=2)
  abline(0,0,lty=2, col="blue",lwd=2)
  # Teste de não correlação    
  dwtest(mod_corpo, alternative = "two.sided")
  
  # sabor
  
  tsi <- rstudent(mod_sabor);a <- max(tsi);b <- min(tsi)
  plot(tsi, pch=16, xlab="Índice", ylab="Resíduo Studentizado",ylim=c(b-1,a+1))
  abline(-2,0,lty=2, col="red", lwd=2)
  abline(2,0,lty=2, col="red", lwd=2)
  abline(0,0,lty=2, col="blue",lwd=2)
  # Teste de não correlação    
  dwtest(mod_sabor, alternative = "two.sided")  
  
  tsi <- rstudent(mod_sabor1);a <- max(tsi);b <- min(tsi)
  plot(tsi, pch=16, xlab="Índice", ylab="Resíduo Studentizado",ylim=c(b-1,a+1))
  abline(-2,0,lty=2, col="red", lwd=2)
  abline(2,0,lty=2, col="red", lwd=2)
  abline(0,0,lty=2, col="blue",lwd=2)
  # Teste de não correlação    
  dwtest(mod_sabor1, alternative = "two.sided")
  
  tsi <- rstudent(mod_sabor2);a <- max(tsi);b <- min(tsi)
  plot(tsi, pch=16, xlab="Índice", ylab="Resíduo Studentizado",ylim=c(b-1,a+1))
  abline(-2,0,lty=2, col="red", lwd=2)
  abline(2,0,lty=2, col="red", lwd=2)
  abline(0,0,lty=2, col="blue",lwd=2)
  # Teste de não correlação    
  dwtest(mod_sabor2, alternative = "two.sided") 
  
  tsi <- rstudent(mod_sabor3);a <- max(tsi);b <- min(tsi)
  plot(tsi, pch=16, xlab="Índice", ylab="Resíduo Studentizado",ylim=c(b-1,a+1))
  abline(-2,0,lty=2, col="red", lwd=2)
  abline(2,0,lty=2, col="red", lwd=2)
  abline(0,0,lty=2, col="blue",lwd=2)
  # Teste de não correlação    
  dwtest(mod_sabor3, alternative = "two.sided") 
  
  
  # alavancagem
  
  # aroma 1
  
  plot(lm.influence(mod_aroma1)$hat, pch=16, xlab="Índice", ylab="Medida h", ylim=c(0,0.22))
  abline(4/length(aroma),0,lty=2, col="blue", lwd=2)
  abline(6/length(aroma),0,lty=2, col="red", lwd=2)
  legend(34,0.22,"4/n",col="blue",lty=2,lwd=2,bty="n")
  legend(34,0.21,"6/n",col="red",lty=2,lwd=2,bty="n")
  
  identify(lm.influence(mod_aroma1)$hat)
  
  # aroma 2
  
  plot(lm.influence(mod_aroma2)$hat, pch=16, xlab="Índice", ylab="Medida h", ylim=c(0,0.22))
  abline(4/length(aroma),0,lty=2, col="blue", lwd=2)
  abline(6/length(aroma),0,lty=2, col="red", lwd=2)
  legend(34,0.22,"4/n",col="blue",lty=2,lwd=2,bty="n")
  legend(34,0.21,"6/n",col="red",lty=2,lwd=2,bty="n")
  
  identify(lm.influence(mod_aroma2)$hat)
  
  # corpo
  
  plot(lm.influence(mod_corpo)$hat, pch=16, xlab="Índice", ylab="Medida h", ylim=c(0,0.22))
  abline(4/length(corpo),0,lty=2, col="blue", lwd=2)
  abline(6/length(corpo),0,lty=2, col="red", lwd=2)
  legend(34,0.22,"4/n",col="blue",lty=2,lwd=2,bty="n")
  legend(34,0.21,"6/n",col="red",lty=2,lwd=2,bty="n")
  
  identify(lm.influence(mod_corpo)$hat)  
  
  
  # Influência -------------------------------------------------
  # aroma1 ----
  n <- length(aroma)
  
  # Distâncias de Cook
  plot(cooks.distance(mod_aroma1), pch=16, xlab="índice", ylab="Distância de Cook")
  abline(4/n,0,lty=2,lwd=2, col="blue")
  identify(cooks.distance(mod_aroma1)) # 20
  
  # DFFITS
  plot(abs(dffits(mod_aroma1)), pch=16, xlab="índice", ylab="DFFITS")
  abline(2*sqrt(2/n),0,lty=2,lwd=2, col="blue")
  identify(abs(dffits(mod_aroma1))) 
  
  # DFBETAS - Intercepto
  plot(abs(dfbetas(mod_aroma1)[,1]), pch=16, xlab="índice", ylab="DFBETAS - intercepto")
  abline(2/sqrt(n),0,lty=2,lwd=2, col="blue")
  identify(abs(dfbetas(mod_aroma1)[,1])) 
  
  # DFBETAS - Coeficiente ang.
  plot(abs(dfbetas(mod_aroma1)[,2]), pch=16, xlab="índice", ylab="DFBETAS - Coef. angular")
  abline(2/sqrt(n),0,lty=2,lwd=2, col="blue")
  identify(abs(dfbetas(mod_aroma1)[,2])) 
  
  # aroma1 ----
  n <- length(aroma)
  
  # Distâncias de Cook
  plot(cooks.distance(mod_aroma1), pch=16, xlab="índice", ylab="Distância de Cook")
  abline(4/n,0,lty=2,lwd=2, col="blue")
  identify(cooks.distance(mod_aroma1)) # 20
  
  # DFFITS
  plot(abs(dffits(mod_aroma1)), pch=16, xlab="índice", ylab="DFFITS")
  abline(2*sqrt(2/n),0,lty=2,lwd=2, col="blue")
  identify(abs(dffits(mod_aroma1))) 
  
  # DFBETAS - Intercepto
  plot(abs(dfbetas(mod_aroma1)[,1]), pch=16, xlab="índice", ylab="DFBETAS - intercepto")
  abline(2/sqrt(n),0,lty=2,lwd=2, col="blue")
  identify(abs(dfbetas(mod_aroma1)[,1])) 
  
  # DFBETAS - Coeficiente ang.
  plot(abs(dfbetas(mod_aroma1)[,2]), pch=16, xlab="índice", ylab="DFBETAS - Coef. angular")
  abline(2/sqrt(n),0,lty=2,lwd=2, col="blue")
  identify(abs(dfbetas(mod_aroma1)[,2])) 
  
  
  # corpo ----
  n <- length(corpo)
  
  # Distâncias de Cook
  plot(cooks.distance(mod_aroma1), pch=16, xlab="índice", ylab="Distância de Cook")
  abline(4/n,0,lty=2,lwd=2, col="blue")
  identify(cooks.distance(mod_aroma1)) 
  
  # DFFITS
  plot(abs(dffits(mod_corpo)), pch=16, xlab="índice", ylab="DFFITS")
  abline(2*sqrt(2/n),0,lty=2,lwd=2, col="blue")
  identify(abs(dffits(mod_corpo))) 
  
  # DFBETAS - Intercepto
  plot(abs(dfbetas(mod_corpo)[,1]), pch=16, xlab="índice", ylab="DFBETAS - intercepto")
  abline(2/sqrt(n),0,lty=2,lwd=2, col="blue")
  identify(abs(dfbetas(mod_corpo)[,1])) 
  
  # DFBETAS - Coeficiente ang.
  plot(abs(dfbetas(mod_corpo)[,2]), pch=16, xlab="índice", ylab="DFBETAS - Coeficiente ang.")
  abline(2/sqrt(n),0,lty=2,lwd=2, col="blue")
  identify(abs(dfbetas(mod_corpo)[,2])) 
  
  # investigação dos pontos atípicos
  
  MR <- function(a,b) cat("Mudança relativa = ", round(as.numeric((b/a-1)*100),2), "%","\n")
  
  # aroma1 ------
  summary(mod_aroma1)
  aroma1_sem12 <- lm(log(qualidade) ~ aroma, subset = -c(12)); summary(aroma1_sem12)
  MR(coef(mod_aroma1), coef(aroma1_sem12))
  
  aroma1_sem14 <- lm(log(qualidade) ~ aroma, subset = -c(14)); summary(aroma1_sem14)
  MR(coef(mod_aroma1), coef(aroma1_sem14))
  
  aroma1_sem15 <- lm(log(qualidade) ~ aroma, subset = -c(15)); summary(aroma1_sem15)
  MR(coef(mod_aroma1), coef(aroma1_sem15))
  
  aroma1_sem20 <- lm(log(qualidade) ~ aroma, subset = -c(20)); summary(aroma1_sem20)
  MR(coef(mod_aroma1), coef(aroma1_sem20))
  
  aroma1_sem30 <- lm(log(qualidade) ~ aroma, subset = -c(30)); summary(aroma1_sem30)
  MR(coef(mod_aroma1), coef(aroma1_sem30))
  
  aroma1_sem33 <- lm(log(qualidade) ~ aroma, subset = -c(33)); summary(aroma1_sem33)
  MR(coef(mod_aroma1), coef(aroma1_sem33))
  
  aroma1_sem34 <- lm(log(qualidade) ~ aroma, subset = -c(34)); summary(aroma1_sem34)
  MR(coef(mod_aroma1), coef(aroma1_sem34))
  
  aroma1_sem_todos <- lm(log(qualidade) ~ aroma, subset = -c(12, 14, 15, 20, 30, 33, 34)); summary(aroma1_sem_todos)
  MR(coef(mod_aroma1), coef(aroma1_sem_todos))
  
  # corpo ------
  summary(mod_corpo)
  corpo_sem1 <- lm(qualidade ~ corpo, subset = -c(1)); summary(corpo_sem1)
  MR(coef(mod_corpo), coef(corpo_sem1))
  
  corpo_sem4 <- lm(qualidade ~ corpo, subset = -c(4)); summary(corpo_sem4)
  MR(coef(mod_corpo), coef(corpo_sem4))
  
  corpo_sem14 <- lm(qualidade ~ corpo, subset = -c(14)); summary(corpo_sem14)
  MR(coef(mod_corpo), coef(corpo_sem14))
  
  corpo_sem15 <- lm(qualidade ~ corpo, subset = -c(15)); summary(corpo_sem15)
  MR(coef(mod_corpo), coef(corpo_sem15))
  
  corpo_sem20 <- lm(qualidade ~ corpo, subset = -c(20)); summary(corpo_sem20)
  MR(coef(mod_corpo), coef(corpo_sem20))
  
  corpo_sem30 <- lm(qualidade ~ corpo, subset = -c(30)); summary(corpo_sem30)
  MR(coef(mod_corpo), coef(corpo_sem30))
  
  corpo_sem37 <- lm(qualidade ~ corpo, subset = -c(37)); summary(corpo_sem37)
  MR(coef(mod_corpo), coef(corpo_sem37))
  
  corpo_sem_todos <- lm(qualidade ~ corpo, subset = -c(1, 4, 14, 15, 20, 30, 37)); summary(corpo_sem_todos)
  MR(coef(mod_corpo), coef(corpo_sem_todos))
  
  # intervalo de confiança
  
  confint(mod_aroma1, level = 0.95)
  