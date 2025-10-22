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
  
  library(MASS)
  boxlife <- boxcox(mod_aroma, plotit = TRUE)
  lambda1 <- boxlife$x[which.max(boxlife$y)]
  lambda1

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
  