# LISTA PRÁTICA 1 DE ANÁLISE DE REGRESSÃO LINEAR - 2025.1

setwd("C:/Users/marin/Documents/YAY/Regressão/Lista Prática 1")

library(readr)
library(tidyverse)
require(car)
require(MASS)
require(lmtest)
#install.packages("glmtoolbox")
require(glmtoolbox)

UNLE <- read_csv("UNLifeExpectancy.csv")
View(UNLifeExpectancy)
dados = UNLE
attach(dados)

# Nosso interesse aqui é explicar a "taxa de fecundidade" dos países em função de uma única 
# variável escolhida a partir demais variáveis disponíveis. As variáveis que podem ser 
# utilizadas individualmente para explicar a taxa de fecundidade s˜ao:


# Variáveis -----------------------------------------------------------------------------------------------------

# LIFEEXP: Expectativa de vida ao nascer, em anos;
# ILLITERATE: Taxa de analfabetismo adulto, % da populaç˜ao com 15 anos ou mais;
# PRIVATEHEALTH: Gasto privado com saúde em 2004, % do PIB;
# PUBLICEDUCATION: Gasto público com educaç˜ao, % do PIB;
# HEALTHEXPEND: Gasto com saúde per capita em 2004, PPA em dólares americanos;
# BIRTHATTEND: Nascimentos assistidos por profissionais de saúde qualificados (%);
# PHYSICIAN: Médicos por 100.000 habitantes;
# SMOKING: Prevalência de tabagismo (masculino), % de adultos;
# RESEARCHERS: Pesquisadores em Pesquisa e Desenvolvimento, por milh˜ao de habitantes;
# GDP: Produto Interno Bruto, em bilh˜oes de dólares americanos;
# FEMALEBOSS: Legisladoras, altas funcionárias e gerentes, % do sexo feminino. 

#  REGION, COUNTRY e POP não serão usados


# Explicações para mim mesma ------------------------------------------------------------------------------------
        # - Na estatística e em machine learning, o verbo "fit" (em inglês) significa "ajustar um modelo" aos dados. 
        # Entao é comum nomear o objeto que guarda o "modelo ajustado" com algo como fit, para lembrar que ele 
        # contém o modelo "ajustado" (fitted).

        # - A funçao lm() significa linear model (modelo linear).
          # Estamos ajustando um modelo de regressao linear simples, onde:
            #  - temos a variável dependente (resposta)
            #  - e a variável independente (previsora)


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

# FERTILITY
summary(dados$FERTILITY)
cat("Desvio padrão:", sd(dados$FERTILITY, na.rm = TRUE), "\n")
cat("Valores ausentes:", sum(is.na(dados$FERTILITY)), "\n")

  # Histograma
  ggplot(dados) +
    aes(x = FERTILITY) +
    geom_histogram(colour = "white", fill = "#1F77B4", binwidth = 1) +
    labs(
      x = "Expectativa de Vida ao Nascer (anos)", 
      y = "Frequência Absoluta",
      title = "Histograma da variável FERTILITY"
    ) +
    theme_estat()+
    theme(plot.title = element_text(hjust = 0.5))
  
  # Boxplot
  ggplot(dados) +
    aes(x = factor(""), y = FERTILITY) +
    geom_boxplot(fill = "#1F77B4", width = 0.3) + 
    guides(fill = FALSE) +
    stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "white") +
    labs(
      x = "", 
      y = "Taxa de Fecundidade"
    ) +
    theme_estat() +
    theme(plot.title = element_text(hjust = 0.5))
  
  ggsave("box_fertility.pdf", width = 158, height = 93, units = "mm")
  

# LIFEEXP
summary(dados$LIFEEXP)
cat("Desvio padrão:", sd(dados$LIFEEXP, na.rm = TRUE), "\n")
cat("Valores ausentes:", sum(is.na(dados$LIFEEXP)), "\n")

    # Histograma
    ggplot(dados) +
      aes(x = LIFEEXP) +
      geom_histogram(colour = "white", fill = "#1F77B4", binwidth = 2) +
      labs(
        x = "Expectativa de Vida ao Nascer (anos)", 
        y = "Frequência Absoluta"
      ) +
      theme_estat()+
      theme(plot.title = element_text(hjust = 0.5))
    
    
    # Boxplot
    ggplot(dados) +
      aes(x = factor(""), y = LIFEEXP) +
      geom_boxplot(fill = "#1F77B4", width = 0.3) + 
      guides(fill = FALSE) +
      stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "white") +
      labs(
        x = "", 
        y = "Expectativa de Vida ao Nascer (anos)"
      ) +
      theme_estat() +
      theme(plot.title = element_text(hjust = 0.5))
    
    ggsave("box_lifeexp.pdf", width = 158, height = 93, units = "mm")


# ILLITERATE
summary(dados$ILLITERATE)
cat("Desvio padrão:", sd(dados$ILLITERATE, na.rm = TRUE), "\n")
cat("Valores ausentes:", sum(is.na(dados$ILLITERATE)), "\n")

    # Histograma
    ggplot(dados) +
      aes(x = ILLITERATE) +
      geom_histogram(colour = "white", fill = "#1F77B4", binwidth = 5) +
      labs(
        x = "Taxa de analfabetismo adulto, % da populaçao com 15 anos ou mais", 
        y = "Frequência Absoluta"
      ) +
      theme_estat()+
      theme(plot.title = element_text(hjust = 0.5))
    
    # Boxplot
    ggplot(dados) +
      aes(x = factor(""), y = ILLITERATE) +
      geom_boxplot(fill = "#1F77B4", width = 0.3) + 
      guides(fill = FALSE) +
      stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "white") +
      labs(
        x = "", 
        y = "Taxa de analfabetismo adulto, \n% da populaçao com 15 anos ou mais"
      ) +
      theme_estat() +
      theme(plot.title = element_text(hjust = 0.5))
    
    ggsave("box_illiterate.pdf", width = 158, height = 93, units = "mm")
    
    
# PRIVATEHEALTH
summary(dados$PRIVATEHEALTH)
cat("Desvio padrão:", sd(dados$PRIVATEHEALTH, na.rm = TRUE), "\n")
cat("Valores ausentes:", sum(is.na(dados$PRIVATEHEALTH)), "\n")

    # Histograma
    ggplot(dados) +
      aes(x = PRIVATEHEALTH) +
      geom_histogram(colour = "white", fill = "#1F77B4", binwidth = 1) +
      labs(
        x = "Gasto privado com saúde em 2004, % do PIB", 
        y = "Frequência Absoluta"
      ) +
      theme_estat()+
      theme(plot.title = element_text(hjust = 0.5))
    
    # Boxplot
    ggplot(dados) +
      aes(x = factor(""), y = PRIVATEHEALTH) +
      geom_boxplot(fill = "#1F77B4", width = 0.3) + 
      guides(fill = FALSE) +
      stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "white") +
      labs(
        x = "", 
        y = "Gasto privado com saúde em 2004, \n% do PIB"
      ) +
      theme_estat() +
      theme(plot.title = element_text(hjust = 0.5))


    ggsave("box_privatehealth.pdf", width = 158, height = 93, units = "mm")
    
    
# PUBLICEDUCATION
summary(dados$PUBLICEDUCATION)
cat("Desvio padrão:", sd(dados$PUBLICEDUCATION, na.rm = TRUE), "\n")
cat("Valores ausentes:", sum(is.na(dados$PUBLICEDUCATION)), "\n")

    # Histograma
    ggplot(dados) +
      aes(x = PUBLICEDUCATION) +
      geom_histogram(colour = "white", fill = "#1F77B4", binwidth = 2) +
      labs(
        x = "Gasto público com educaç˜a, % do PIBo", 
        y = "Frequência Absoluta"
      ) +
      theme_estat()+
      theme(plot.title = element_text(hjust = 0.5))
    
    # Boxplot
    ggplot(dados) +
      aes(x = factor(""), y = PUBLICEDUCATION) +
      geom_boxplot(fill = "#1F77B4", width = 0.3) + 
      guides(fill = FALSE) +
      stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "white") +
      labs(
        x = "", 
        y = "Gasto público com educaçao, \n% do PIBo"
      ) +
      theme_estat() +
      theme(plot.title = element_text(hjust = 0.5))

    ggsave("box_publiceducation.pdf", width = 158, height = 93, units = "mm")
    
# HEALTHEXPEND
summary(dados$HEALTHEXPEND)
cat("Desvio padrão:", sd(dados$HEALTHEXPEND, na.rm = TRUE), "\n")
cat("Valores ausentes:", sum(is.na(dados$HEALTHEXPEND)), "\n")

    # Histograma
    ggplot(dados) +
      aes(x = HEALTHEXPEND) +
      geom_histogram(colour = "white", fill = "#1F77B4", binwidth = 300) +
      labs(
        x = "Gasto com saúde per capita em 2004", 
        y = "Frequência Absoluta"
      ) +
      theme_estat()+
      theme(plot.title = element_text(hjust = 0.5))
    
    
    # Boxplot
    ggplot(dados) +
      aes(x = factor(""), y = HEALTHEXPEND) +
      geom_boxplot(fill = "#1F77B4", width = 0.3) + 
      guides(fill = FALSE) +
      stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "white") +
      labs(
        x = "", 
        y = "Gasto com saúde per capita em 2004"
      ) +
      theme_estat() +
      theme(plot.title = element_text(hjust = 0.5))

    ggsave("box_heathexpend.pdf", width = 158, height = 93, units = "mm")
    
    
# BIRTHATTEND
summary(dados$BIRTHATTEND)
cat("Desvio padrão:", sd(dados$BIRTHATTEND, na.rm = TRUE), "\n")
cat("Valores ausentes:", sum(is.na(dados$BIRTHATTEND)), "\n")

    # Histograma
    ggplot(dados) +
      aes(x = BIRTHATTEND) +
      geom_histogram(colour = "white", fill = "#1F77B4", binwidth = 5) +
      labs(
        x = "Nascimentos assistidos por profissionais de saúde qualificados (%)", 
        y = "Frequência Absoluta"
      ) +
      theme_estat()+
      theme(plot.title = element_text(hjust = 0.5))
    
    
    # Boxplot
    ggplot(dados) +
      aes(x = factor(""), y = BIRTHATTEND) +
      geom_boxplot(fill = "#1F77B4", width = 0.3) + 
      guides(fill = FALSE) +
      stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "white") +
      labs(
        x = "", 
        y = "Nascimentos assistidos por profissionais \nde saúde qualificados (%)"
      ) +
      theme_estat() +
      theme(plot.title = element_text(hjust = 0.5))
    
    ggsave("box_birthattend.pdf", width = 158, height = 93, units = "mm")
# PHYSICIAN
summary(dados$PHYSICIAN)
cat("Desvio padrão:", sd(dados$PHYSICIAN, na.rm = TRUE), "\n")
cat("Valores ausentes:", sum(is.na(dados$PHYSICIAN)), "\n")

    # Histograma
    ggplot(dados) +
      aes(x = PHYSICIAN) +
      geom_histogram(colour = "white", fill = "#1F77B4", binwidth = 20) +
      labs(
        x = " Médicos por 100.000 habitantes", 
        y = "Frequência Absoluta"
      ) +
      theme_estat()+
      theme(plot.title = element_text(hjust = 0.5))
    
    
    # Boxplot
    ggplot(dados) +
      aes(x = factor(""), y = PHYSICIAN) +
      geom_boxplot(fill = "#1F77B4", width = 0.3) + 
      guides(fill = FALSE) +
      stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "white") +
      labs(
        x = "", 
        y = "Médicos por 100.000 habitantes"
      ) +
      theme_estat() +
      theme(plot.title = element_text(hjust = 0.5))

    ggsave("box_physician.pdf", width = 158, height = 93, units = "mm")
    
# SMOKING
summary(dados$SMOKING)
cat("Desvio padrão:", sd(dados$SMOKING, na.rm = TRUE), "\n")
cat("Valores ausentes:", sum(is.na(dados$SMOKING)), "\n")

    # Histograma
    ggplot(dados) +
      aes(x = SMOKING) +
      geom_histogram(colour = "white", fill = "#1F77B4", binwidth = 5) +
      labs(
        x = "Prevalência de tabagismo (masculino),% de adultos", 
        y = "Frequência Absoluta"
      ) +
      theme_estat()+
      theme(plot.title = element_text(hjust = 0.5))
    
    
    # Boxplot
    ggplot(dados) +
      aes(x = factor(""), y = SMOKING) +
      geom_boxplot(fill = "#1F77B4", width = 0.3) + 
      guides(fill = FALSE) +
      stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "white") +
      labs(
        x = "", 
        y = "Prevalência de tabagismo (masculino),\n % de adultos"
      ) +
      theme_estat() +
      theme(plot.title = element_text(hjust = 0.5))

    ggsave("box_smoking.pdf", width = 158, height = 93, units = "mm")
    
# RESEARCHERS
summary(dados$RESEARCHERS)
cat("Desvio padrão:", sd(dados$RESEARCHERS, na.rm = TRUE), "\n")
cat("Valores ausentes:", sum(is.na(dados$RESEARCHERS)), "\n")

    # Histograma
    ggplot(dados) +
      aes(x = RESEARCHERS) +
      geom_histogram(colour = "white", fill = "#1F77B4", binwidth = 1000) +
      labs(
        x = "Pesquisadores em Pesquisa e Desenvolvimento, por milhao de habitante", 
        y = "Frequência Absoluta"
      ) +
      theme_estat()+
      theme(plot.title = element_text(hjust = 0.5))
    
    
    # Boxplot
    ggplot(dados) +
      aes(x = factor(""), y = RESEARCHERS) +
      geom_boxplot(fill = "#1F77B4", width = 0.3) + 
      guides(fill = FALSE) +
      stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "white") +
      labs(
        x = "", 
        y = "Pesquisadores em Pesquisa e \nDesenvolvimento, por milhao de habitante"
      ) +
      theme_estat() +
      theme(plot.title = element_text(hjust = 0.5))

    ggsave("box_researchers.pdf", width = 158, height = 93, units = "mm")
# GDP
summary(dados$GDP)
cat("Desvio padrão:", sd(dados$GDP, na.rm = TRUE), "\n")
cat("Valores ausentes:", sum(is.na(dados$GDP)), "\n")

    # Histograma
    ggplot(dados) +
      aes(x = GDP) +
      geom_histogram(colour = "white", fill = "#1F77B4", binwidth = 1000) +
      labs(
        x = "Produto Interno Bruto, em bilhoes de dólares", 
        y = "Frequência Absoluta"
      ) +
      theme_estat()+
      theme(plot.title = element_text(hjust = 0.5))
    
    
    # Boxplot
    ggplot(dados) +
      aes(x = factor(""), y = GDP) +
      geom_boxplot(fill = "#1F77B4", width = 0.3) + 
      guides(fill = FALSE) +
      stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "white") +
      labs(
        x = "", 
        y = "Produto Interno Bruto, em bilhoes de dólares "
      ) +
      theme_estat() +
      theme(plot.title = element_text(hjust = 0.5))

    ggsave("box_gdp.pdf", width = 158, height = 93, units = "mm")

# FEMALEBOSS
summary(dados$FEMALEBOSS)
cat("Desvio padrão:", sd(dados$FEMALEBOSS, na.rm = TRUE), "\n")
cat("Valores ausentes:", sum(is.na(dados$FEMALEBOSS)), "\n")

    # Histograma
    ggplot(dados) +
      aes(x = FEMALEBOSS) +
      geom_histogram(colour = "white", fill = "#1F77B4", binwidth = 5) +
      labs(
        x = "Legisladoras, altas funcionárias e gerentes, % do sexo feminino", 
        y = "Frequência Absoluta"
      ) +
      theme_estat()+
      theme(plot.title = element_text(hjust = 0.5))
    
    
    # Boxplot
    ggplot(dados) +
      aes(x = factor(""), y = FEMALEBOSS) +
      geom_boxplot(fill = "#1F77B4", width = 0.3) + 
      guides(fill = FALSE) +
      stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "white") +
      labs(
        x = "", 
        y = "Legisladoras, altas funcionárias e gerentes, \n% do sexo feminino"
      ) +
      theme_estat() +
      theme(plot.title = element_text(hjust = 0.5))

    ggsave("box_femaleboss.pdf", width = 158, height = 93, units = "mm")
    
## Gráficos de dispersao da variável de interesse com cada covariável -------------------------------------------

# LIFEEXP: 
    
    ggplot(dados, aes(x = LIFEEXP, y = FERTILITY)) +
      geom_point(shape = 16, size = 2) +                     # Pontos
      geom_smooth(method = "lm", se = FALSE, color = "#1F77B4") + # Linha de regressão linear sem intervalo de confiança
      labs(
        x = "Expectativa de vida ao nascer, em anos",
        y = "Taxa de Fecundidade"
      ) +
      meu_tema
    
    ggsave("disp_lifeexp.pdf", width = 158, height = 93, units = "mm")
    
# ILLITERATE: 
    
    ggplot(dados, aes(x = ILLITERATE, y = FERTILITY)) +
      geom_point(shape = 16, size = 2) +                     # Pontos
      geom_smooth(method = "lm", se = FALSE, color = "#1F77B4") + # Linha de regressão linear sem intervalo de confiança
      labs(
        x = "Taxa de analfabetismo adulto, % da populaçao \ncom 15 anos ou mais",
        y = "Taxa de Fecundidade"
      ) +
      meu_tema
    
    ggsave("disp_illiterate.pdf", width = 158, height = 93, units = "mm")
   
# PRIVATEHEALTH: 
    
    ggplot(dados, aes(x = PRIVATEHEALTH, y = FERTILITY)) +
      geom_point(shape = 16, size = 2) +                     # Pontos
      geom_smooth(method = "lm", se = FALSE, color = "#1F77B4") + # Linha de regressão linear sem intervalo de confiança
      labs(
        x = "Gasto privado com saúde em 2004, % do PIB",
        y = "Taxa de Fecundidade"
      ) +
      meu_tema

    ggsave("disp_privatehealth.pdf", width = 158, height = 93, units = "mm")
# PUBLICEDUCATION:

    ggplot(dados, aes(x = PUBLICEDUCATION, y = FERTILITY)) +
      geom_point(shape = 16, size = 2) +                     # Pontos
      geom_smooth(method = "lm", se = FALSE, color = "#1F77B4") + # Linha de regressão linear sem intervalo de confiança
      labs(
        x = "Gasto público com educaçao, % do PIB",
        y = "Taxa de Fecundidade"
      ) +
      meu_tema 
   
    ggsave("disp_publiceducation.pdf", width = 158, height = 93, units = "mm")
# HEALTHEXPEND:
    
    ggplot(dados, aes(x = HEALTHEXPEND, y = FERTILITY)) +
      geom_point(shape = 16, size = 2) +                     # Pontos
      geom_smooth(method = "lm", se = FALSE, color = "#1F77B4") + # Linha de regressão linear sem intervalo de confiança
      labs(
        x = "Gasto com saúde per capita em 2004",
        y = "Taxa de Fecundidade"
      ) + 
      scale_y_continuous(limits = c(1, NA)) +
      meu_tema
    
    ggsave("disp_healthexpend.pdf", width = 158, height = 93, units = "mm")
   
# BIRTHATTEND: 
 
    ggplot(dados, aes(x = BIRTHATTEND, y = FERTILITY)) +
      geom_point(shape = 16, size = 2) +                     # Pontos
      geom_smooth(method = "lm", se = FALSE, color = "#1F77B4") + # Linha de regressão linear sem intervalo de confiança
      labs(
        x = "Nascimentos assistidos por profissionais \n de saúde qualificados (%)",
        y = "Taxa de Fecundidade"
      ) +
      meu_tema
    
    ggsave("disp_birthattend.pdf", width = 158, height = 93, units = "mm")
   
# PHYSICIAN:
      
    ggplot(dados, aes(x = PHYSICIAN, y = FERTILITY)) +
      geom_point(shape = 16, size = 2,  color = "black") +                     # Pontos
      geom_smooth(method = "lm", se = FALSE, color = "#1F77B4") + # Linha de regressão linear sem intervalo de confiança
      labs(
        x = "Médicos por 100.000 habitantes",
        y = "Taxa de Fecundidade"
      ) +
      scale_y_continuous(limits = c(0.8, NA)) +
      meu_tema
    
    ggsave("disp_physician.pdf", width = 158, height = 93, units = "mm")
   
# SMOKING: 

    ggplot(dados, aes(x = SMOKING, y = FERTILITY)) +
      geom_point(shape = 16, size = 2) +                     # Pontos
      geom_smooth(method = "lm", se = FALSE, color = "#1F77B4") + # Linha de regressão linear sem intervalo de confiança
      labs(
        x = "Prevalência de tabagismo (masculino), % de adultos",
        y = "Taxa de Fecundidade"
      ) +
      meu_tema
    
    ggsave("disp_smoking.pdf", width = 158, height = 93, units = "mm")
    
# RESEARCHERS: 

    ggplot(dados, aes(x = RESEARCHERS, y = FERTILITY)) +
      geom_point(shape = 16, size = 2) +                     # Pontos
      geom_smooth(method = "lm", se = FALSE, color = "#1F77B4") + # Linha de regressão linear sem intervalo de confiança
      labs(
        x = "Pesquisadores em Pesquisa e Desenvolvimento, por milhao de habitantes",
        y = "Taxa de Fecundidade"
      ) +
      meu_tema
    
    ggsave("disp_researdhers.pdf", width = 158, height = 93, units = "mm")
    
# GDP:

    ggplot(dados, aes(x = GDP, y = FERTILITY)) +
      geom_point(shape = 16, size = 2) +                     # Pontos
      geom_smooth(method = "lm", se = FALSE, color = "#1F77B4") + # Linha de regressão linear sem intervalo de confiança
      labs(
        x = "Produto Interno Bruto, em bilhoes de dólares",
        y = "Taxa de Fecundidade"
      ) +
      scale_y_continuous(limits = c(0.8, NA)) +
      meu_tema
    
    ggsave("disp_gdp.pdf", width = 158, height = 93, units = "mm")
   
# FEMALEBOSS:
      
    ggplot(dados, aes(x = FEMALEBOSS, y = FERTILITY)) +
      geom_point(shape = 16, size = 2) +                     # Pontos
      geom_smooth(method = "lm", se = FALSE, color = "#1F77B4") + # Linha de regressão linear sem intervalo de confiança
      labs(
        x = "Legisladoras, altas funcionárias e gerentes, % do sexo feminino",
        y = "Taxa de Fecundidade"
      ) +
      scale_y_continuous(limits = c(0.8, NA)) +
      meu_tema

    ggsave("disp_femaleboss.pdf", width = 158, height = 93, units = "mm")

    
    
# ----
#*********************************************** Transformações *********************************************************#
## Trasformação das variáveis (para facilitar a vizualição e ajudar para deixá-la mais linear possivel) ---------   

  ###Explicação 
      # λ = 1: sem transformação
      # λ = 0: log(resposta)
      # λ = 0.5: raiz quadrada
      # λ = -1: inverso    
      

# LIFEEXP: ----
    boxcox(FERTILITY~LIFEEXP, data = dados, plotit = T) #modelo com regressao
    boxlife = boxcox(FERTILITY~LIFEEXP, data = dados ,plotit = F)
    lambda1 = boxlife$x[boxlife$y == max(boxlife$y)]; lambda1
  
    # sem transformação
    ggplot(dados, aes(x = LIFEEXP, y = FERTILITY)) +
      geom_point(shape = 16, size = 2) +                     # Pontos
      geom_smooth(method = "lm", se = FALSE, color = "#1F77B4") + # Linha de regressão linear sem intervalo de confiança
      labs(
        x = "Expectativa de vida ao nascer, em anos",
        y = "Taxa de Fecundidade"
      ) +
      meu_tema
    
    ggsave("LIFEEXP_semtransformacao.pdf", width = 158, height = 93, units = "mm")
    
    # Boxcox  
    ggplot(dados, aes(x = LIFEEXP, y = sqrt(FERTILITY))) +
      geom_point(shape = 16, size = 2) +                     # Pontos
      geom_smooth(method = "lm", se = FALSE, color = "#1F77B4") + # Linha de regressão linear sem intervalo de confiança
      labs(
        x = "Expectativa de vida ao nascer, em anos",
        y = "Raiz quadrada da Taxa  \nde Fecundidade"
      ) +
      meu_tema
    
    ggsave("LIFEEXP_raizquad.pdf", width = 158, height = 93, units = "mm")
    
    # Log na resposta 
    ggplot(dados, aes(x = LIFEEXP, y = log(FERTILITY))) +
      geom_point(shape = 16, size = 2) +                     # Pontos
      #geom_smooth(method = "lm", se = FALSE, color = "#1F77B4") + # Linha de regressão linear sem intervalo de confiança
      labs(
        x = "Expectativa de vida ao nascer, em anos",
        y = "Log da Taxa de Fecundidade"
      ) +
      meu_tema
    
    #ggsave("LIFEEXP_logres.pdf", width = 158, height = 93, units = "mm")
    
    # Log na explicativa
    ggplot(dados, aes(x = log(LIFEEXP), y = FERTILITY)) +
      geom_point(shape = 16, size = 2) +                     # Pontos
      geom_smooth(method = "lm", se = FALSE, color = "#1F77B4") + # Linha de regressão linear sem intervalo de confiança
      labs(
        x = "Log da Expectativa de vida ao nascer, em anos",
        y = "Taxa de Fecundidade"
      ) +
      meu_tema
    
    ggsave("LIFEEXP_logexp.pdf", width = 158, height = 93, units = "mm")
    
    # Log nas duas variáveis
    ggplot(dados, aes(x = log(LIFEEXP), y = log(FERTILITY))) +
      geom_point(shape = 16, size = 2) +                     # Pontos
     # geom_smooth(method = "lm", se = FALSE, color = "#1F77B4") + # Linha de regressão linear sem intervalo de confiança
      labs(
        x = "Log da Expectativa de vida ao nascer, em anos",
        y = "Log da Taxa de Fecundidade"
      ) +
      meu_tema
    
    # ggsave("LIFEEXP_logduas.pdf", width = 158, height = 93, units = "mm")

    
    
# ILLITERATE: ----
    boxcox(FERTILITY~ILLITERATE, data = dados, plotit = T) #modelo com regressao
    boxlife = boxcox(FERTILITY~ILLITERATE, data = dados ,plotit = F)
    lambda1 = boxlife$x[boxlife$y == max(boxlife$y)]; lambda1
    
    # Sem transformação
    ggplot(dados, aes(x = ILLITERATE, y = FERTILITY)) +
      geom_point(shape = 16, size = 2) +                     # Pontos
      geom_smooth(method = "lm", se = FALSE, color = "#1F77B4") + # Linha de regressão linear sem intervalo de confiança
      labs(
        x = "Taxa de analfabetismo adulto, \n% da populaçao com 15 anos ou mais",
        y = "Taxa de Fecundidade"
      ) +
      meu_tema
    
    ggsave("ILLITERATE_semtrans.pdf", width = 158, height = 93, units = "mm")
    # Boxcox  
    ggplot(dados, aes(x = ILLITERATE, y = sqrt(FERTILITY))) +
      geom_point(shape = 16, size = 2) +                     # Pontos
      geom_smooth(method = "lm", se = FALSE, color = "#1F77B4") + # Linha de regressão linear sem intervalo de confiança
      labs(
        x = "Taxa de analfabetismo adulto, \n% da populaçao com 15 anos ou mais",
        y = "Raiz quadrada da \n Taxa de Fecundidade"
      ) +
      meu_tema
    
    ggsave("ILLITERATE_raizquad.pdf", width = 158, height = 93, units = "mm")
    
    # Log na resposta 
    ggplot(dados, aes(x = ILLITERATE, y = log(FERTILITY))) +
      geom_point(shape = 16, size = 2) +                     # Pontos
      #geom_smooth(method = "lm", se = FALSE, color = "#1F77B4") + # Linha de regressão linear sem intervalo de confiança
      labs(
        x = "Taxa de analfabetismo adulto, % da populaçao com 15 anos ou mais",
        y = "Taxa de Fecundidade"
      ) +
      meu_tema
    
    # Log na explicativa
    ggplot(dados, aes(x = log(ILLITERATE), y = FERTILITY)) +
      geom_point(shape = 16, size = 2) +                     # Pontos
      #geom_smooth(method = "lm", se = FALSE, color = "#1F77B4") + # Linha de regressão linear sem intervalo de confiança
      labs(
        x = "Taxa de analfabetismo adulto, % da populaçao com 15 anos ou mais",
        y = "Taxa de Fecundidade"
      ) +
      meu_tema
    
        #DE ANTEMÃO, ESSE ESTÁ MUITOOO RUIM
    
    # Log nas duas variáveis
    ggplot(dados, aes(x = log(ILLITERATE), y = log(FERTILITY))) +
      geom_point(shape = 16, size = 2) +                     # Pontos
      #geom_smooth(method = "lm", se = FALSE, color = "#1F77B4") + # Linha de regressão linear sem intervalo de confiança
      labs(
        x = "Taxa de analfabetismo adulto, % da populaçao com 15 anos ou mais",
        y = "Taxa de Fecundidade"
      ) +
      meu_tema
    
          #DE ANTEMÃO, ESSE ESTÁ MUITOOO RUIM
    

    
# PRIVATEHEALTH:----
    boxcox(FERTILITY~PRIVATEHEALTH, data = dados, plotit = T) #modelo com regressao
    boxlife = boxcox(FERTILITY~PRIVATEHEALTH, data = dados ,plotit = F)
    lambda1 = boxlife$x[boxlife$y == max(boxlife$y)]; lambda1
    
    #*************** Essa variáveç está MUITO ruim ***************
      
    # Sem transformação
    ggplot(dados, aes(x = PRIVATEHEALTH, y = FERTILITY)) +
      geom_point(shape = 16, size = 2) +                     # Pontos
      #geom_smooth(method = "lm", se = FALSE, color = "#1F77B4") + # Linha de regressão linear sem intervalo de confiança
      labs(
        x = "Gasto privado com saúde em 2004, % do PIB",
        y = "Taxa de Fecundidade"
      ) +
      meu_tema
    
    # Boxcox
    ggplot(dados, aes(x = PRIVATEHEALTH, y = log(FERTILITY))) +
      geom_point(shape = 16, size = 2) +                     # Pontos
      #geom_smooth(method = "lm", se = FALSE, color = "#1F77B4") + # Linha de regressão linear sem intervalo de confiança
      labs(
        x = "Gasto privado com saúde em 2004, % do PIB",
        y = "Taxa de Fecundidade"
      ) +
      meu_tema
    
    # Log na resposta 
    ggplot(dados, aes(x = PRIVATEHEALTH, y = log(FERTILITY))) +
      geom_point(shape = 16, size = 2) +                     # Pontos
      #geom_smooth(method = "lm", se = FALSE, color = "#1F77B4") + # Linha de regressão linear sem intervalo de confiança
      labs(
        x = "Gasto privado com saúde em 2004, % do PIB",
        y = "Taxa de Fecundidade"
      ) +
      meu_tema
    
    # Log na explicativa
    ggplot(dados, aes(x = log(PRIVATEHEALTH), y = FERTILITY)) +
      geom_point(shape = 16, size = 2) +                     # Pontos
      #geom_smooth(method = "lm", se = FALSE, color = "#1F77B4") + # Linha de regressão linear sem intervalo de confiança
      labs(
        x = "Gasto privado com saúde em 2004, % do PIB",
        y = "Taxa de Fecundidade"
      ) +
      meu_tema
    
    # Log nas duas variáveis
    ggplot(dados, aes(x = log(PRIVATEHEALTH), y = log(FERTILITY))) +
      geom_point(shape = 16, size = 2) +                     # Pontos
      #geom_smooth(method = "lm", se = FALSE, color = "#1F77B4") + # Linha de regressão linear sem intervalo de confiança
      labs(
        x = "Gasto privado com saúde em 2004, % do PIB",
        y = "Taxa de Fecundidade"
      ) +
      meu_tema
        
        # Essa é a melhor da variável PRIVATEHEALTH
    
    
# PUBLICEDUCATION:----
    boxcox(FERTILITY~PUBLICEDUCATION, data = dados, plotit = T) #modelo com regressao
    boxlife = boxcox(FERTILITY~PUBLICEDUCATION, data = dados ,plotit = F)
    lambda1 = boxlife$x[boxlife$y == max(boxlife$y)]; lambda1
    
    #*************** Essa variáveç está MUITO ruim ***************
    
    # Sem transformação
    ggplot(dados, aes(x = PUBLICEDUCATION, y = FERTILITY)) +
      geom_point(shape = 16, size = 2) +                     # Pontos
      #geom_smooth(method = "lm", se = FALSE, color = "#1F77B4") + # Linha de regressão linear sem intervalo de confiança
      labs(
        x = "Gasto público com educaçao, % do PIB",
        y = "Taxa de Fecundidade"
      ) +
      meu_tema 
    
    # Boxcox
    ggplot(dados, aes(x = PUBLICEDUCATION, y = log(FERTILITY))) +
      geom_point(shape = 16, size = 2) +                     # Pontos
      #geom_smooth(method = "lm", se = FALSE, color = "#1F77B4") + # Linha de regressão linear sem intervalo de confiança
      labs(
        x = "Gasto público com educaçao, % do PIB",
        y = "Taxa de Fecundidade"
      ) +
      meu_tema 
    
    # Log na resposta
    ggplot(dados, aes(x = PUBLICEDUCATION, y = log(FERTILITY))) +
      geom_point(shape = 16, size = 2) +                     # Pontos
      #geom_smooth(method = "lm", se = FALSE, color = "#1F77B4") + # Linha de regressão linear sem intervalo de confiança
      labs(
        x = "Gasto público com educaçao, % do PIB",
        y = "Taxa de Fecundidade"
      ) +
      meu_tema 
    
    
    # Log na explicativa
    ggplot(dados, aes(x = log(PUBLICEDUCATION), y = FERTILITY)) +
      geom_point(shape = 16, size = 2) +                     # Pontos
      #geom_smooth(method = "lm", se = FALSE, color = "#1F77B4") + # Linha de regressão linear sem intervalo de confiança
      labs(
        x = "Gasto público com educaçao, % do PIB",
        y = "Taxa de Fecundidade"
      ) +
      meu_tema 
    
    # Log nas duas variáveis
    ggplot(dados, aes(x = log(PUBLICEDUCATION), y = log(FERTILITY))) +
      geom_point(shape = 16, size = 2) +                     # Pontos
      #geom_smooth(method = "lm", se = FALSE, color = "#1F77B4") + # Linha de regressão linear sem intervalo de confiança
      labs(
        x = "Gasto público com educaçao, % do PIB",
        y = "Taxa de Fecundidade"
      ) +
      meu_tema 
    
    
# HEALTHEXPEND:----
    boxcox(FERTILITY~HEALTHEXPEND, data = dados, plotit = T) #modelo com regressao
    boxlife = boxcox(FERTILITY~HEALTHEXPEND, data = dados ,plotit = F)
    lambda1 = boxlife$x[boxlife$y == max(boxlife$y)]; lambda1
    
    # Sem transformação 
    ggplot(dados, aes(x = HEALTHEXPEND, y = FERTILITY)) +
      geom_point(shape = 16, size = 2) +                     # Pontos
      #geom_smooth(method = "lm", se = FALSE, color = "#1F77B4") + # Linha de regressão linear sem intervalo de confiança
      labs(
        x = "Gasto com saúde per capita em 2004",
        y = "Taxa de Fecundidade"
      ) + 
      scale_y_continuous(limits = c(1, NA)) +
      meu_tema
    
  
    # Boxcox
    ggplot(dados, aes(x = HEALTHEXPEND, y = log(FERTILITY))) +
      geom_point(shape = 16, size = 2) +                     # Pontos
      #geom_smooth(method = "lm", se = FALSE, color = "#1F77B4") + # Linha de regressão linear sem intervalo de confiança
      labs(
        x = "Gasto com saúde per capita em 2004",
        y = "Taxa de Fecundidade"
      ) + 
      scale_y_continuous(limits = c(1, NA)) +
      meu_tema
    
    # Log na resposta 
    ggplot(dados, aes(x = HEALTHEXPEND, y = log(FERTILITY))) +
      geom_point(shape = 16, size = 2) +                     # Pontos
      #geom_smooth(method = "lm", se = FALSE, color = "#1F77B4") + # Linha de regressão linear sem intervalo de confiança
      labs(
        x = "Gasto com saúde per capita em 2004",
        y = "Taxa de Fecundidade"
      ) + 
      scale_y_continuous(limits = c(1, NA)) +
      meu_tema
    
    # Log na explicativa
    ggplot(dados, aes(x = log(HEALTHEXPEND), y = FERTILITY)) +
      geom_point(shape = 16, size = 2) +                     # Pontos
      geom_smooth(method = "lm", se = FALSE, color = "#1F77B4") + # Linha de regressão linear sem intervalo de confiança
      labs(
        x = "Log do gasto com saúde per capita em 2004",
        y = "Taxa de Fecundidade"
      ) + 
      scale_y_continuous(limits = c(1, NA)) +
      meu_tema 
    
    ggsave("HEALTHEXPEND_logexp.pdf", width = 158, height = 93, units = "mm")
    
    # Log nas duas variáveis
    ggplot(dados, aes(x = log(HEALTHEXPEND), y = log(FERTILITY))) +
      geom_point(shape = 16, size = 2) +                     # Pontos
      geom_smooth(method = "lm", se = FALSE, color = "#1F77B4") + # Linha de regressão linear sem intervalo de confiança
      labs(
        x = "Log do gasto com saúde per capita em 2004",
        y = "Log da taxa de Fecundidade"
      ) + 
      scale_y_continuous(limits = c(1, NA)) +
      meu_tema
    
    ggsave("HEALTHEXPEND_logdois.pdf", width = 158, height = 93, units = "mm")
        # Essa ficou muito boa 
    
# BIRTHATTEND: ----
    boxcox(FERTILITY~BIRTHATTEND, data = dados, plotit = T) #modelo com regressao
    boxlife = boxcox(FERTILITY~BIRTHATTEND, data = dados ,plotit = F)
    lambda1 = boxlife$x[boxlife$y == max(boxlife$y)]; lambda1

    #*************** Essa variáveç está EXTREMAMENTE ruim ***************    

    # Sem transformação
    ggplot(dados, aes(x = BIRTHATTEND, y = FERTILITY)) +
      geom_point(shape = 16, size = 2) +                     # Pontos
      #geom_smooth(method = "lm", se = FALSE, color = "#1F77B4") + # Linha de regressão linear sem intervalo de confiança
      labs(
        x = "Nascimentos assistidos por profissionais de saúde qualificados (%)",
        y = "Taxa de Fecundidade"
      ) +
      meu_tema
    
    # Boxcox
    ggplot(dados, aes(x = BIRTHATTEND, y = log(FERTILITY))) +
      geom_point(shape = 16, size = 2) +                     # Pontos
      #geom_smooth(method = "lm", se = FALSE, color = "#1F77B4") + # Linha de regressão linear sem intervalo de confiança
      labs(
        x = "Nascimentos assistidos por profissionais de saúde qualificados (%)",
        y = "Taxa de Fecundidade"
      ) +
      meu_tema
    
    # Log na resposta 
    ggplot(dados, aes(x = BIRTHATTEND, y = log(FERTILITY))) +
      geom_point(shape = 16, size = 2) +                     # Pontos
      #geom_smooth(method = "lm", se = FALSE, color = "#1F77B4") + # Linha de regressão linear sem intervalo de confiança
      labs(
        x = "Nascimentos assistidos por profissionais de saúde qualificados (%)",
        y = "Taxa de Fecundidade"
      ) +
      meu_tema
    
    # Log na explicativa
    ggplot(dados, aes(x = log(BIRTHATTEND), y = FERTILITY)) +
      geom_point(shape = 16, size = 2) +                     # Pontos
      #geom_smooth(method = "lm", se = FALSE, color = "#1F77B4") + # Linha de regressão linear sem intervalo de confiança
      labs(
        x = "Nascimentos assistidos por profissionais de saúde qualificados (%)",
        y = "Taxa de Fecundidade"
      ) +
      meu_tema
    
    # Log nas duas variáveis
    ggplot(dados, aes(x = log(BIRTHATTEND), y = log(FERTILITY))) +
      geom_point(shape = 16, size = 2) +                     # Pontos
      #geom_smooth(method = "lm", se = FALSE, color = "#1F77B4") + # Linha de regressão linear sem intervalo de confiança
      labs(
        x = "Nascimentos assistidos por profissionais de saúde qualificados (%)",
        y = "Taxa de Fecundidade"
      ) +
      meu_tema
  
# PHYSICIAN: ----
    boxcox(FERTILITY~PHYSICIAN, data = dados, plotit = T) #modelo com regressao
    boxlife = boxcox(FERTILITY~PHYSICIAN, data = dados ,plotit = F)
    lambda1 = boxlife$x[boxlife$y == max(boxlife$y)]; lambda1
    
    # Sem transformação
    ggplot(dados, aes(x = PHYSICIAN, y = FERTILITY)) +
      geom_point(shape = 16, size = 2,  color = "black") +                     # Pontos
      #geom_smooth(method = "lm", se = FALSE, color = "#1F77B4") + # Linha de regressão linear sem intervalo de confiança
      labs(
        x = "Médicos por 100.000 habitantes",
        y = "Taxa de Fecundidade"
      ) +
      scale_y_continuous(limits = c(0.8, NA)) +
      meu_tema
    
    # Boxcox
    ggplot(dados, aes(x = PHYSICIAN, y = sqrt(FERTILITY))) +
      geom_point(shape = 16, size = 2,  color = "black") +                     # Pontos
      #geom_smooth(method = "lm", se = FALSE, color = "#1F77B4") + # Linha de regressão linear sem intervalo de confiança
      labs(
        x = "Médicos por 100.000 habitantes",
        y = "Taxa de Fecundidade"
      ) +
      scale_y_continuous(limits = c(0.8, NA)) +
      meu_tema
    
    # Log na resposta 
    ggplot(dados, aes(x = PHYSICIAN, y = log(FERTILITY))) +
      geom_point(shape = 16, size = 2,  color = "black") +                     # Pontos
      #geom_smooth(method = "lm", se = FALSE, color = "#1F77B4") + # Linha de regressão linear sem intervalo de confiança
      labs(
        x = "Médicos por 100.000 habitantes",
        y = "Taxa de Fecundidade"
      ) +
      scale_y_continuous(limits = c(0.8, NA)) +
      meu_tema
    # Log na explicativa 
    ggplot(dados, aes(x = log(PHYSICIAN), y = FERTILITY)) +
      geom_point(shape = 16, size = 2,  color = "black") +                     # Pontos
      geom_smooth(method = "lm", se = FALSE, color = "#1F77B4") + # Linha de regressão linear sem intervalo de confiança
      labs(
        x = "Log na quantidade de médicos \n por 100.000 habitantes",
        y = "Taxa de Fecundidade"
      ) +
      scale_y_continuous(limits = c(0.8, NA)) +
      meu_tema
    
    ggsave("PHYSICIAN_logexp.pdf", width = 158, height = 93, units = "mm")
        # Essa transformação está MUITO interessante
    
    # Log nas duas variáveis
    ggplot(dados, aes(x = log(PHYSICIAN), y = sqrt(FERTILITY))) +
      geom_point(shape = 16, size = 2,  color = "black") +                     # Pontos
      geom_smooth(method = "lm", se = FALSE, color = "#1F77B4") + # Linha de regressão linear sem intervalo de confiança
      labs(
        x = "Log na quantidade de médicos \n por 100.000 habitantes",
        y = "Log na taxa de Fecundidade"
      ) +
      scale_y_continuous(limits = c(1, NA)) +
      meu_tema
    
    ggsave("PHYSICIAN_logdois.pdf", width = 158, height = 93, units = "mm")
        # Essa transformação está MUITO interessante
    
# SMOKING: ----
    boxcox(FERTILITY~SMOKING, data = dados, plotit = T) #modelo com regressao
    boxlife = boxcox(FERTILITY~SMOKING, data = dados ,plotit = F)
    lambda1 = boxlife$x[boxlife$y == max(boxlife$y)]; lambda1
    
    # Sem transformação
    ggplot(dados, aes(x = SMOKING, y = FERTILITY)) +
      geom_point(shape = 16, size = 2) +                     # Pontos
      #geom_smooth(method = "lm", se = FALSE, color = "#1F77B4") + # Linha de regressão linear sem intervalo de confiança
      labs(
        x = "Prevalência de tabagismo (masculino), % de adultos",
        y = "Taxa de Fecundidade"
      ) +
      meu_tema
    
    
    # Boxcox
    ggplot(dados, aes(x = SMOKING, y = sqrt(FERTILITY))) +
      geom_point(shape = 16, size = 2) +                     # Pontos
      #geom_smooth(method = "lm", se = FALSE, color = "#1F77B4") + # Linha de regressão linear sem intervalo de confiança
      labs(
        x = "Prevalência de tabagismo (masculino), % de adultos",
        y = "Taxa de Fecundidade"
      ) +
      meu_tema
    
    # Log na resposta 
    ggplot(dados, aes(x = SMOKING, y = log(FERTILITY))) +
      geom_point(shape = 16, size = 2) +                     # Pontos
      #geom_smooth(method = "lm", se = FALSE, color = "#1F77B4") + # Linha de regressão linear sem intervalo de confiança
      labs(
        x = "Prevalência de tabagismo (masculino), % de adultos",
        y = "Taxa de Fecundidade"
      ) +
      meu_tema
    
    # Log na explicativa
    ggplot(dados, aes(x = log(SMOKING), y = FERTILITY)) +
      geom_point(shape = 16, size = 2) +                     # Pontos
      #geom_smooth(method = "lm", se = FALSE, color = "#1F77B4") + # Linha de regressão linear sem intervalo de confiança
      labs(
        x = "Prevalência de tabagismo (masculino), % de adultos",
        y = "Taxa de Fecundidade"
      ) +
      meu_tema
    
        # Muito ruim
    
    # Log nas duas variáveis 
    ggplot(dados, aes(x = log(SMOKING), y = log(FERTILITY))) +
      geom_point(shape = 16, size = 2) +                     # Pontos
      #geom_smooth(method = "lm", se = FALSE, color = "#1F77B4") + # Linha de regressão linear sem intervalo de confiança
      labs(
        x = "Prevalência de tabagismo (masculino), % de adultos",
        y = "Taxa de Fecundidade"
      ) +
      meu_tema
    
# RESEARCHERS: ----
    boxcox(FERTILITY~RESEARCHERS, data = dados, plotit = T) #modelo com regressao
    boxlife = boxcox(FERTILITY~RESEARCHERS, data = dados ,plotit = F)
    lambda1 = boxlife$x[boxlife$y == max(boxlife$y)]; lambda1
   
    # Sem transformação 
    ggplot(dados, aes(x = RESEARCHERS, y = FERTILITY)) +
      geom_point(shape = 16, size = 2) +                     # Pontos
      #geom_smooth(method = "lm", se = FALSE, color = "#1F77B4") + # Linha de regressão linear sem intervalo de confiança
      labs(
        x = "Pesquisadores em Pesquisa e Desenvolvimento, por milhao de habitantes",
        y = "Taxa de Fecundidade"
      ) +
      meu_tema
   
    # Boxcox 
    ggplot(dados, aes(x = RESEARCHERS, y = 1/FERTILITY)) +
      geom_point(shape = 16, size = 2) +                     # Pontos
      #geom_smooth(method = "lm", se = FALSE, color = "#1F77B4") + # Linha de regressão linear sem intervalo de confiança
      labs(
        x = "Pesquisadores em Pesquisa e Desenvolvimento, por milhao de habitantes",
        y = "Inversa da Taxa de Fecundidade"
      ) +
      meu_tema
    
    # Log na resposta 
    ggplot(dados, aes(x = RESEARCHERS, y = log(FERTILITY))) +
      geom_point(shape = 16, size = 2) +                     # Pontos
      #geom_smooth(method = "lm", se = FALSE, color = "#1F77B4") + # Linha de regressão linear sem intervalo de confiança
      labs(
        x = "Pesquisadores em Pesquisa e Desenvolvimento, por milhao de habitantes",
        y = "Taxa de Fecundidade"
      ) +
      meu_tema
    
    # Log na explicativa
    ggplot(dados, aes(x = log(RESEARCHERS), y = FERTILITY)) +
      geom_point(shape = 16, size = 2) +                     # Pontos
      #geom_smooth(method = "lm", se = FALSE, color = "#1F77B4") + # Linha de regressão linear sem intervalo de confiança
      labs(
        x = "Pesquisadores em Pesquisa e Desenvolvimento, por milhao de habitantes",
        y = "Taxa de Fecundidade"
      ) +
      meu_tema
    
    # Log nas duas variáveis
    ggplot(dados, aes(x = log(RESEARCHERS), y = log(FERTILITY))) +
      geom_point(shape = 16, size = 2) +                     # Pontos
      geom_smooth(method = "lm", se = FALSE, color = "#1F77B4") + # Linha de regressão linear sem intervalo de confiança
      labs(
        x = "Log da quantidade de Pesquisadores em Pesquisa e Desenvolvimento, por milhao de habitantes",
        y = "Log da taxa de Fecundidade"
      ) +
      meu_tema
    
    ggsave("RESEARCHERS_logdois.pdf", width = 158, height = 93, units = "mm")
      # Esse e a transformação de cima estão legais, o resto tá uma bosta
    
# GDP: ----
    boxcox(FERTILITY~GDP, data = dados, plotit = T) #modelo com regressao
    boxlife = boxcox(FERTILITY~GDP, data = dados ,plotit = F)
    lambda1 = boxlife$x[boxlife$y == max(boxlife$y)]; lambda1
    
    #*************** Essa variável está MUITO ruim (tirando com o log na explicativa) ****************
    #*
    # Sem transformação
    ggplot(dados, aes(x = GDP, y = FERTILITY)) +
      geom_point(shape = 16, size = 2) +                     # Pontos
      #geom_smooth(method = "lm", se = FALSE, color = "#1F77B4") + # Linha de regressão linear sem intervalo de confiança
      labs(
        x = "Produto Interno Bruto, em bilhoes de dólares",
        y = "Taxa de Fecundidade"
      ) +
      scale_y_continuous(limits = c(0.8, NA)) +
      meu_tema
    
    
    # Boxcox
    ggplot(dados, aes(x = GDP, y = log(FERTILITY))) +
      geom_point(shape = 16, size = 2) +                     # Pontos
      #geom_smooth(method = "lm", se = FALSE, color = "#1F77B4") + # Linha de regressão linear sem intervalo de confiança
      labs(
        x = "Produto Interno Bruto, em bilhoes de dólares",
        y = "Taxa de Fecundidade"
      ) +
      scale_y_continuous(limits = c(0.8, NA)) +
      meu_tema
    
    # Log na resposta 
    ggplot(dados, aes(x = GDP, y = log(FERTILITY))) +
      geom_point(shape = 16, size = 2) +                     # Pontos
      #geom_smooth(method = "lm", se = FALSE, color = "#1F77B4") + # Linha de regressão linear sem intervalo de confiança
      labs(
        x = "Produto Interno Bruto, em bilhoes de dólares",
        y = "Taxa de Fecundidade"
      ) +
      scale_y_continuous(limits = c(0.8, NA)) +
      meu_tema
    
    # Log na explicativa
    ggplot(dados, aes(x = log(GDP), y = FERTILITY)) +
      geom_point(shape = 16, size = 2) +                     # Pontos
      geom_smooth(method = "lm", se = FALSE, color = "#1F77B4") + # Linha de regressão linear sem intervalo de confiança
      labs(
        x = "Log do Produto Interno Bruto, em bilhoes de dólares",
        y = "Taxa de Fecundidade"
      ) +
      scale_y_continuous(limits = c(0.8, NA)) +
      meu_tema
    
    ggsave("GDP_logexp.pdf", width = 158, height = 93, units = "mm")
    
    #Essa daqui é interessante, possivel escolha
    
    # Log nas duas variáveis
    ggplot(dados, aes(x = GDP, y = log(FERTILITY))) +
      geom_point(shape = 16, size = 2) +                     # Pontos
      #geom_smooth(method = "lm", se = FALSE, color = "#1F77B4") + # Linha de regressão linear sem intervalo de confiança
      labs(
        x = "Produto Interno Bruto, em bilhoes de dólares",
        y = "Taxa de Fecundidade"
      ) +
      scale_y_continuous(limits = c(0.8, NA)) +
      meu_tema
    
    
    
# FEMALEBOSS: ----
    boxcox(FERTILITY~FEMALEBOSS, data = dados, plotit = T) #modelo com regressao
    boxlife = boxcox(FERTILITY~FEMALEBOSS, data = dados ,plotit = F)
    lambda1 = boxlife$x[boxlife$y == max(boxlife$y)]; lambda1
    
    #*************** Essa variável está MUITO ruim ***************
    
    # Sem transformação
    ggplot(dados, aes(x = FEMALEBOSS, y = FERTILITY)) +
      geom_point(shape = 16, size = 2) +                     # Pontos
      #geom_smooth(method = "lm", se = FALSE, color = "#1F77B4") + # Linha de regressão linear sem intervalo de confiança
      labs(
        x = "Legisladoras, altas funcionárias e gerentes, % do sexo feminino",
        y = "Taxa de Fecundidade"
      ) +
      scale_y_continuous(limits = c(0.8, NA)) +
      meu_tema
    
    
    # Boxcox
    ggplot(dados, aes(x = FEMALEBOSS, y = sqrt(FERTILITY))) +
      geom_point(shape = 16, size = 2) +                     # Pontos
      #geom_smooth(method = "lm", se = FALSE, color = "#1F77B4") + # Linha de regressão linear sem intervalo de confiança
      labs(
        x = "Legisladoras, altas funcionárias e gerentes, % do sexo feminino",
        y = "Taxa de Fecundidade"
      ) +
      scale_y_continuous(limits = c(0.8, NA)) +
      meu_tema
    
    # Log na resposta 
    ggplot(dados, aes(x = FEMALEBOSS, y = log(FERTILITY))) +
      geom_point(shape = 16, size = 2) +                     # Pontos
      #geom_smooth(method = "lm", se = FALSE, color = "#1F77B4") + # Linha de regressão linear sem intervalo de confiança
      labs(
        x = "Legisladoras, altas funcionárias e gerentes, % do sexo feminino",
        y = "Taxa de Fecundidade"
      ) +
      scale_y_continuous(limits = c(0.8, NA)) +
      meu_tema
    
    # Log na explicativa
    ggplot(dados, aes(x = log(FEMALEBOSS), y = FERTILITY)) +
      geom_point(shape = 16, size = 2) +                     # Pontos
      #geom_smooth(method = "lm", se = FALSE, color = "#1F77B4") + # Linha de regressão linear sem intervalo de confiança
      labs(
        x = "Legisladoras, altas funcionárias e gerentes, % do sexo feminino",
        y = "Taxa de Fecundidade"
      ) +
      scale_y_continuous(limits = c(0.8, NA)) +
      meu_tema
    
    # Log nas duas variáveis
    ggplot(dados, aes(x = log(FEMALEBOSS), y = log(FERTILITY))) +
      geom_point(shape = 16, size = 2) +                     # Pontos
      #geom_smooth(method = "lm", se = FALSE, color = "#1F77B4") + # Linha de regressão linear sem intervalo de confiança
      labs(
        x = "Legisladoras, altas funcionárias e gerentes, % do sexo feminino",
        y = "Taxa de Fecundidade"
      ) +
      scale_y_continuous(limits = c(0.8, NA)) +
      meu_tema
    
    
    
    
    
    
# LISTA DAS TRANSFORMACOES ESCOLHIDAS ---------------------------------------------------------------------------------------------------------------------
    
    #*1 Sem transformação - LIFEEXP
    #*2 Raiz quadrada - LIFEEXP
    #*3 Log da explicativa - LIFEEXP (talvez)
    #*4 Sem transformação - ILLITERATE
    #*5 Raiz quadrada - ILLITERATE
    #*6 Log da explicativa - HEALTHEXPEND
    #*7 Log das duas variáveis - HEALTHEXPEND
    #*8 Log da explicativa - PHYSICIAN
    #*9 Log das duas variáveis - PHYSICIAN
    #*10 Log das duas variáveis - RESERCHERS 
    #*11 Log da explicativa - GPD
  
    
# Ajustes do Modelo de Regressao Linear Simples para as transformações escolhidas ----------------------------------------------------------------
 
    #* Vamos aproveitar para escolher os melhores modelos e as melhores transformações
    
    # Sem transformação - LIFEEXP
    ajuste1 <- lm(FERTILITY ~ LIFEEXP, data = dados); summary(ajuste1)
    
    # Raiz quadrada - LIFEEXP
    ajuste2 <- lm(sqrt(FERTILITY) ~ LIFEEXP, data = dados); summary(ajuste2)
    
    # Log da explicativa - LIFEEXP (talvez)
    ajuste3 <- lm(FERTILITY ~ log(LIFEEXP), data = dados); summary(ajuste3)
    
    # Sem transformação - ILLITERATE
    ajuste4 <- lm(FERTILITY ~ ILLITERATE, data = dados); summary(ajuste4)
    
    # Raiz quadrada - ILLITERATE
    ajuste5 <- lm(sqrt(FERTILITY) ~ ILLITERATE, data = dados); summary(ajuste5)
    
    # Log da explicativa - HEALTHEXPEND
    ajuste6 <- lm(FERTILITY ~ log(HEALTHEXPEND), data = dados); summary(ajuste6)
    
    # Log das duas variáveis - HEALTHEXPEND
    ajuste7 <- lm(log(FERTILITY) ~ log(HEALTHEXPEND), data = dados); summary(ajuste7)
    
    # Log da explicativa - PHYSICIAN
    ajuste8 <- lm(FERTILITY ~ log(PHYSICIAN), data = dados); summary(ajuste8)
    
    # Log das duas variáveis - PHYSICIAN
    ajuste9 <- lm(log(FERTILITY) ~ log(PHYSICIAN), data = dados); summary(ajuste9)
    
    # Log das duas variáveis - RESERCHERS
    ajuste10 <- lm(log(FERTILITY) ~ log(RESEARCHERS), data = dados); summary(ajuste10)
    
    # Log da explicativa - GPD
    ajuste11 <- lm(FERTILITY ~ log(GDP), data = dados); summary(ajuste11)
    
    
    
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

    
# Calculando os testes para as transformações escolhidas
    
    # Ajuste 1 N ----
    ## Normalidade 
    envelope_LR(ajuste1,  main.title = "")
      # Teste de Normalidade
      shapiro.test(rstudent(ajuste1))

    ## Homogeneidade
    tsi <- rstudent(ajuste1);a <- max(tsi);b <- min(tsi)
    
    plot(fitted(ajuste1),tsi,xlab="Valor Ajustado",ylab="Resíduo Studentizado", ylim=c(b-1,a+1), pch=16)
    abline(-2,0,lty=2, col="red", lwd=2)
    abline(2,0,lty=2, col="red", lwd=2)
    abline(0,0,lty=2, col="blue",lwd=2)
      # Teste de homocedasticidade 
    gqtest(ajuste1, fraction=1/3, order.by=model.frame(ajuste1)$LIFEEXP, alternative="two.sided")
    
    ## Não correlação dos erros
    tsi <- rstudent(ajuste1);a <- max(tsi);b <- min(tsi)
    plot(tsi, pch=16, xlab="índice", ylab="Resíduo Studentizado",ylim=c(b-1,a+1))
    abline(-2,0,lty=2, col="red", lwd=2)
    abline(2,0,lty=2, col="red", lwd=2)
    abline(0,0,lty=2, col="blue",lwd=2)
      # Teste de não correlação    
      dwtest(ajuste1, alternative = "two.sided")
      
      
    # Ajuste 2 N ----
    ## Normalidade 
    envelope_LR(ajuste2,  main.title = "")
      # Teste de Normalidade
      shapiro.test(rstudent(ajuste2))
      
    ## Homogeneidade
    tsi <- rstudent(ajuste2);a <- max(tsi);b <- min(tsi)
    plot(fitted(ajuste2),tsi,xlab="Valor Ajustado",ylab="Resíduo Studentizado", ylim=c(b-1,a+1), pch=16)
    abline(-2,0,lty=2, col="red", lwd=2)
    abline(2,0,lty=2, col="red", lwd=2)
    abline(0,0,lty=2, col="blue",lwd=2)
      # Teste de homocedasticidade 
    gqtest(ajuste2, fraction=1/3, order.by=model.frame(ajuste2)$LIFEEXP, alternative="two.sided")
    
    ## Não correlação dos erros
    tsi <- rstudent(ajuste2);a <- max(tsi);b <- min(tsi)
    plot(tsi, pch=16, xlab="índice", ylab="Resíduo Studentizado",ylim=c(b-1,a+1))
    abline(-2,0,lty=2, col="red", lwd=2)
    abline(2,0,lty=2, col="red", lwd=2)
    abline(0,0,lty=2, col="blue",lwd=2)
      # Teste de não correlação    
      dwtest(ajuste2, alternative = "two.sided")  
      
      
    # Ajuste 3 N - rejeita a normalidade ----
      ## Normalidade 
      envelope_LR(ajuste3,  main.title = "")
      # Teste de Normalidade
      shapiro.test(rstudent(ajuste3))
      
      ## Homogeneidade
      tsi <- rstudent(ajuste3);a <- max(tsi);b <- min(tsi)
      plot(fitted(ajuste3),tsi,xlab="Valor Ajustado",ylab="Resíduo Studentizado", ylim=c(b-1,a+1), pch=16)
      abline(-2,0,lty=2, col="red", lwd=2)
      abline(2,0,lty=2, col="red", lwd=2)
      abline(0,0,lty=2, col="blue",lwd=2)
      # Teste de homocedasticidade 
      gqtest(ajuste3, fraction=1/3, order.by=model.frame(ajuste3)$LIFEEXP, alternative="two.sided")
      
      ## Não correlação dos erros
      tsi <- rstudent(ajuste3);a <- max(tsi);b <- min(tsi)
      plot(tsi, pch=16, xlab="índice", ylab="Resíduo Studentizado",ylim=c(b-1,a+1))
      abline(-2,0,lty=2, col="red", lwd=2)
      abline(2,0,lty=2, col="red", lwd=2)
      abline(0,0,lty=2, col="blue",lwd=2)
      # Teste de não correlação    
      dwtest(ajuste3, alternative = "two.sided")   
      
      
    # Ajuste 4 N - rejeita a normalidade ----
      ## Normalidade 
      envelope_LR(ajuste4,  main.title = "")
      # Teste de Normalidade
      shapiro.test(rstudent(ajuste4))
      
      ## Homogeneidade
      tsi <- rstudent(ajuste4);a <- max(tsi);b <- min(tsi)
      plot(fitted(ajuste4),tsi,xlab="Valor Ajustado",ylab="Resíduo Studentizado", ylim=c(b-1,a+1), pch=16)
      abline(-2,0,lty=2, col="red", lwd=2)
      abline(2,0,lty=2, col="red", lwd=2)
      abline(0,0,lty=2, col="blue",lwd=2)
      # Teste de homocedasticidade 
      gqtest(ajuste4, fraction=1/3, order.by=model.frame(ajuste3)$ILLITERATE, alternative="two.sided")
      
      
      ## Não correlação dos erros
      tsi <- rstudent(ajuste4);a <- max(tsi);b <- min(tsi)
      plot(tsi, pch=16, xlab="índice", ylab="Resíduo Studentizado",ylim=c(b-1,a+1))
      abline(-2,0,lty=2, col="red", lwd=2)
      abline(2,0,lty=2, col="red", lwd=2)
      abline(0,0,lty=2, col="blue",lwd=2)
      # Teste de não correlação    
      dwtest(ajuste4, alternative = "two.sided")
      
      
    # Ajuste 5 N - rejeita a normalidade ----
      ## Normalidade 
      envelope_LR(ajuste5,  main.title = "")
      # Teste de Normalidade
      shapiro.test(rstudent(ajuste5))
      
      ## Homogeneidade
      tsi <- rstudent(ajuste5);a <- max(tsi);b <- min(tsi)
      plot(fitted(ajuste5),tsi,xlab="Valor Ajustado",ylab="Resíduo Studentizado", ylim=c(b-1,a+1), pch=16)
      abline(-2,0,lty=2, col="red", lwd=2)
      abline(2,0,lty=2, col="red", lwd=2)
      abline(0,0,lty=2, col="blue",lwd=2)
      # Teste de homocedasticidade 
      gqtest(ajuste5, fraction=1/3, order.by=model.frame(ajuste3)$ILLITERATE, alternative="two.sided")
      
      ## Não correlação dos erros
      tsi <- rstudent(ajuste5);a <- max(tsi);b <- min(tsi)
      plot(tsi, pch=16, xlab="índice", ylab="Resíduo Studentizado",ylim=c(b-1,a+1))
      abline(-2,0,lty=2, col="red", lwd=2)
      abline(2,0,lty=2, col="red", lwd=2)
      abline(0,0,lty=2, col="blue",lwd=2)
      # Teste de não correlação    
      dwtest(ajuste5, alternative = "two.sided")
      
      
    # Ajuste 6 S - Esse aceita tudo pelos testes, a homocedasticidade é meio xoxa  ----
      ## Normalidade 
      envelope_LR(ajuste6,  main.title = "")
      # Teste de Normalidade
      shapiro.test(rstudent(ajuste6))
      
      ## Homogeneidade
      tsi <- rstudent(ajuste6);a <- max(tsi);b <- min(tsi)
      plot(fitted(ajuste6),tsi,xlab="Valor Ajustado",ylab="Resíduo Studentizado", ylim=c(b-1,a+1), pch=16)
      abline(-2,0,lty=2, col="red", lwd=2)
      abline(2,0,lty=2, col="red", lwd=2)
      abline(0,0,lty=2, col="blue",lwd=2)
      # Teste de homocedasticidade 
      gqtest(ajuste6, fraction=1/3, order.by=model.frame(ajuste6)$HEALTHEXPEND, alternative="two.sided")
      
      
      
      ## Não correlação dos erros
      tsi <- rstudent(ajuste6);a <- max(tsi);b <- min(tsi)
      plot(tsi, pch=16, xlab="índice", ylab="Resíduo Studentizado",ylim=c(b-1,a+1))
      abline(-2,0,lty=2, col="red", lwd=2)
      abline(2,0,lty=2, col="red", lwd=2)
      abline(0,0,lty=2, col="blue",lwd=2)
      # Teste de não correlação    
      dwtest(ajuste6, alternative = "two.sided")
    
      
      
    # Ajuste 7 N - rejeita a normalidade ----
      ## Normalidade 
      envelope_LR(ajuste7,  main.title = "")
      # Teste de Normalidade
      shapiro.test(rstudent(ajuste7))
      
      ## Homogeneidade
      tsi <- rstudent(ajuste7);a <- max(tsi);b <- min(tsi)
      plot(fitted(ajuste7),tsi,xlab="Valor Ajustado",ylab="Resíduo Studentizado", ylim=c(b-1,a+1), pch=16)
      abline(-2,0,lty=2, col="red", lwd=2)
      abline(2,0,lty=2, col="red", lwd=2)
      abline(0,0,lty=2, col="blue",lwd=2)
      # Teste de homocedasticidade 
      gqtest(ajuste7, fraction=1/3, order.by=model.frame(ajuste7)$HEALTHEXPEND, alternative="two.sided")
      
      ## Não correlação dos erros
      tsi <- rstudent(ajuste7);a <- max(tsi);b <- min(tsi)
      plot(tsi, pch=16, xlab="índice", ylab="Resíduo Studentizado",ylim=c(b-1,a+1))
      abline(-2,0,lty=2, col="red", lwd=2)
      abline(2,0,lty=2, col="red", lwd=2)
      abline(0,0,lty=2, col="blue",lwd=2)
      # Teste de não correlação    
      dwtest(ajuste7, alternative = "two.sided")
      
      
      
    # Ajuste 8 N - rejeita a normalidade ----
      ## Normalidade 
      envelope_LR(ajuste8,  main.title = "")
      # Teste de Normalidade
      shapiro.test(rstudent(ajuste8))
      
      ## Homogeneidade
      tsi <- rstudent(ajuste8);a <- max(tsi);b <- min(tsi)
      plot(fitted(ajuste8),tsi,xlab="Valor Ajustado",ylab="Resíduo Studentizado", ylim=c(b-1,a+1), pch=16)
      abline(-2,0,lty=2, col="red", lwd=2)
      abline(2,0,lty=2, col="red", lwd=2)
      abline(0,0,lty=2, col="blue",lwd=2)
      # Teste de homocedasticidade 
      gqtest(ajuste8, fraction=1/3, order.by=model.frame(ajuste8)$PHYSICIAN, alternative="two.sided")
      
      ## Não correlação dos erros
      tsi <- rstudent(ajuste8);a <- max(tsi);b <- min(tsi)
      plot(tsi, pch=16, xlab="índice", ylab="Resíduo Studentizado",ylim=c(b-1,a+1))
      abline(-2,0,lty=2, col="red", lwd=2)
      abline(2,0,lty=2, col="red", lwd=2)
      abline(0,0,lty=2, col="blue",lwd=2)
      # Teste de não correlação    
      dwtest(ajuste8, alternative = "two.sided")
      
      
      
    # Ajuste 9 S - ESSE ACEITA TUDOOOOOOOOOOO IHUUUULLLLLLL ----
      ## Normalidade 
      envelope_LR(ajuste9,  main.title = "")
      # Teste de Normalidade
      shapiro.test(rstudent(ajuste9))
      
      ## Homogeneidade
      tsi <- rstudent(ajuste9);a <- max(tsi);b <- min(tsi)
      plot(fitted(ajuste9),tsi,xlab="Valor Ajustado",ylab="Resíduo Studentizado", ylim=c(b-1,a+1), pch=16)
      abline(-2,0,lty=2, col="red", lwd=2)
      abline(2,0,lty=2, col="red", lwd=2)
      abline(0,0,lty=2, col="blue",lwd=2)
      # Teste de homocedasticidade 
      gqtest(ajuste9, fraction=1/3, order.by=model.frame(ajuste9)$PHYSICIAN, alternative="two.sided")
      
      ## Não correlação dos erros
      tsi <- rstudent(ajuste9);a <- max(tsi);b <- min(tsi)
      plot(tsi, pch=16, xlab="índice", ylab="Resíduo Studentizado",ylim=c(b-1,a+1))
      abline(-2,0,lty=2, col="red", lwd=2)
      abline(2,0,lty=2, col="red", lwd=2)
      abline(0,0,lty=2, col="blue",lwd=2)
      # Teste de não correlação    
      dwtest(ajuste9, alternative = "two.sided")
      
      
      
    # Ajuste 10 N - Essa não é homocedastica ----
      ## Normalidade 
      envelope_LR(ajuste10,  main.title = "")
      # Teste de Normalidade
      shapiro.test(rstudent(ajuste10))
      
      ## Homogeneidade
      tsi <- rstudent(ajuste10);a <- max(tsi);b <- min(tsi)
      plot(fitted(ajuste10),tsi,xlab="Valor Ajustado",ylab="Resíduo Studentizado", ylim=c(b-1,a+1), pch=16)
      abline(-2,0,lty=2, col="red", lwd=2)
      abline(2,0,lty=2, col="red", lwd=2)
      abline(0,0,lty=2, col="blue",lwd=2)
      # Teste de homocedasticidade 
      gqtest(ajuste10, fraction=1/3, order.by=model.frame(ajuste10)$RESEARCHERS, alternative="two.sided")
      
      ## Não correlação dos erros
      tsi <- rstudent(ajuste10);a <- max(tsi);b <- min(tsi)
      plot(tsi, pch=16, xlab="índice", ylab="Resíduo Studentizado",ylim=c(b-1,a+1))
      abline(-2,0,lty=2, col="red", lwd=2)
      abline(2,0,lty=2, col="red", lwd=2)
      abline(0,0,lty=2, col="blue",lwd=2)
      # Teste de não correlação    
      dwtest(ajuste10, alternative = "two.sided")
      
      
      
      
    # Ajuste 11 N - rejeita a normalidade ----
      ## Normalidade 
      envelope_LR(ajuste11,  main.title = "")
      # Teste de Normalidade
      shapiro.test(rstudent(ajuste11))
      
      ## Homogeneidade
      tsi <- rstudent(ajuste11);a <- max(tsi);b <- min(tsi)
      plot(fitted(ajuste11),tsi,xlab="Valor Ajustado",ylab="Resíduo Studentizado", ylim=c(b-1,a+1), pch=16)
      abline(-2,0,lty=2, col="red", lwd=2)
      abline(2,0,lty=2, col="red", lwd=2)
      abline(0,0,lty=2, col="blue",lwd=2)
      # Teste de homocedasticidade 
      gqtest(ajuste11, fraction=1/3, order.by=model.frame(ajuste11)$GDP, alternative="two.sided")
      
      ## Não correlação dos erros
      tsi <- rstudent(ajuste11);a <- max(tsi);b <- min(tsi)
      plot(tsi, pch=16, xlab="índice", ylab="Resíduo Studentizado",ylim=c(b-1,a+1))
      abline(-2,0,lty=2, col="red", lwd=2)
      abline(2,0,lty=2, col="red", lwd=2)
      abline(0,0,lty=2, col="blue",lwd=2)
      # Teste de não correlação    
      dwtest(ajuste11, alternative = "two.sided")
      
    
    
    
    
   #a 
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    # homogeneidade das variancias
      #let disse que ele nn funcionou
      gqtest(ajuste1, fraction = 0.5, order.by = na.omit(LIFEEXP), alternative = "greater")
      bptest(ajuste1)
      # p-valor < 0.05: Há heterocedasticidade, e a suposição de variância constante dos resíduos é violada.
      
      # p-valor > 0.05: Não há evidências de heterocedasticidade, ou seja, a variância dos resíduos é constante e o modelo é adequado.
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
      
      
      
      
      
      
      
      
      
      
      
      
      
# ALAVANCAGEM ------------------------------------------------
      # Ajuste 6 ----
      plot(lm.influence(ajuste6)$hat, pch=16, xlab="Índice", ylab="Medida h", ylim=c(0,0.1))
      abline(4/length(HEALTHEXPEND),0,lty=2, col="blue", lwd=2)
      abline(6/length(HEALTHEXPEND),0,lty=2, col="red", lwd=2)
      legend(120,0.09,"4/n",col="blue",lty=2,lwd=2,bty="n")
      legend(120,0.08,"6/n",col="red",lty=2,lwd=2,bty="n")
      
      
      # identify(lm.influence(ajuste6)$hat)  -> PELO R BASE
  
      # Ajuste 9 ----
      
      plot(lm.influence(ajuste9)$hat, pch=16, xlab="Índice", ylab="Medida h", ylim=c(0,0.1))
      abline(4/length(PHYSICIAN),0,lty=2, col="blue", lwd=2)
      abline(6/length(PHYSICIAN),0,lty=2, col="red", lwd=2)
      legend(120,0.09,"4/n",col="blue",lty=2,lwd=2,bty="n")
      legend(120,0.08,"6/n",col="red",lty=2,lwd=2,bty="n")
      
      # identify(lm.influence(ajuste7)$hat)  -> PELO R BASE
      
      
# Influência -------------------------------------------------
      # Ajuste 6 ----
      n <- length(HEALTHEXPEND)
      
      # Distâncias de Cook
      plot(cooks.distance(ajuste6), pch=16, xlab="índice", ylab="Distância de Cook")
      abline(4/n,0,lty=2,lwd=2, col="blue")
      abline(6/n,0,lty=2,lwd=2, col="red")
      
      # DFFITS
      plot(abs(dffits(ajuste6)), pch=16, xlab="índice", ylab="DFFITS")
      abline(2*sqrt(2/n),0,lty=2,lwd=2, col="blue")
      # identify(abs(dffits(fit_pa_simples))) 
      
      
      # DFBETAS - Intercepto
      plot(abs(dfbetas(ajuste6)[,1]), pch=16, xlab="índice", ylab="DFBETAS - intercepto")
      abline(2/sqrt(n),0,lty=2,lwd=2, col="blue")
      
      
      # DFBETAS - Coeficiente ang.
      plot(abs(dfbetas(ajuste6)[,2]), pch=16, xlab="índice", ylab="DFBETAS - Coeficiente ang.")
      abline(2/sqrt(n),0,lty=2,lwd=2, col="blue")
      # identify(abs(dfbetas(fit_pa_simples)[,2])) 
      
      
      # Ajuste 9 ----
      n <- length(PHYSICIAN)
      
      # Distâncias de Cook
      plot(cooks.distance(ajuste9), pch=16, xlab="índice", ylab="Distância de Cook")
      abline(4/n,0,lty=2,lwd=2, col="blue")
      
      
      # DFFITS
      plot(abs(dffits(ajuste9)), pch=16, xlab="índice", ylab="DFFITS")
      abline(2*sqrt(2/n),0,lty=2,lwd=2, col="blue")
      # identify(abs(dffits(fit_pa_simples))) 
      
      
      # DFBETAS - Intercepto
      plot(abs(dfbetas(ajuste9)[,1]), pch=16, xlab="índice", ylab="DFBETAS - intercepto")
      abline(2/sqrt(n),0,lty=2,lwd=2, col="blue")
      
      
      # DFBETAS - Coeficiente ang.
      plot(abs(dfbetas(ajuste9)[,2]), pch=16, xlab="índice", ylab="DFBETAS - Coeficiente ang.")
      abline(2/sqrt(n),0,lty=2,lwd=2, col="blue")
      # identify(abs(dfbetas(fit_pa_simples)[,2])) 
      
      
      
      
      
      
      
      
      
      
# Investigação dos pontos atípicos ---------------------------
      MR <- function(a,b) cat("Mudança relativa = ", round(as.numeric((b/a-1)*100),2), "%","\n")
      # Ajute 6 ------
      summary(ajuste6)
      fit6_sem1 <- lm(FERTILITY ~ log(HEALTHEXPEND), subset = -c(1)); summary(fit6_sem1)
      fit6_sem85 <- lm(FERTILITY ~ log(HEALTHEXPEND), subset = -c(85)); summary(fit6_sem85)
      fit6_sem112 <- lm(FERTILITY ~ log(HEALTHEXPEND), subset = -c(112)); summary(fit6_sem112)
      fit6_sem118 <- lm(FERTILITY ~ log(HEALTHEXPEND), subset = -c(118)); summary(fit6_sem118)
      fit6_sem168 <- lm(FERTILITY ~ log(HEALTHEXPEND), subset = -c(168)); summary(fit6_sem168)
      fit6_sem_todos <- lm(FERTILITY ~ log(HEALTHEXPEND), subset = -c(1, 85, 112, 118, 168)); summary(fit6_sem_todos)
      
      
      MR(coef(ajuste6), coef(fit6_sem1))
      MR(coef(ajuste6), coef(fit6_sem85))
      MR(coef(ajuste6), coef(fit6_sem112))
      MR(coef(ajuste6), coef(fit6_sem118))
      MR(coef(ajuste6), coef(fit6_sem168))
      MR(coef(ajuste6), coef(fit6_sem_todos))

      # Ajute 9 ------
      summary(ajuste9)
      fit9_sem18 <- lm(log(FERTILITY) ~ log(PHYSICIAN), subset = -c(18)); summary(fit6_sem18)
      fit9_sem98 <- lm(log(FERTILITY) ~ log(PHYSICIAN), subset = -c(98)); summary(fit9_sem98)
      fit9_sem118 <- lm(log(FERTILITY) ~ log(PHYSICIAN), subset = -c(118)); summary(fit9_sem118)
      fit9_sem157 <- lm(log(FERTILITY) ~ log(PHYSICIAN), subset = -c(157)); summary(fit9_sem157)
      fit9_sem_todos <- lm(log(FERTILITY) ~ log(PHYSICIAN), subset = -c(18, 98, 118, 157)); summary(fit9_sem_todos)
      
      
      MR(coef(ajuste9), coef(fit9_sem18))
      MR(coef(ajuste9), coef(fit9_sem98))
      MR(coef(ajuste9), coef(fit9_sem118))
      MR(coef(ajuste9), coef(fit9_sem157))
      MR(coef(ajuste9), coef(fit9_sem_todos))
      
      