####### Lista 2 #######

#### pacotes e banco ####
library(ggplot2)
library(MASS)
library(car)
library(mgcv)
library(faraway)
library(lmtest)
require(alr4)
require(GLMsData)
require(olsrr)

attach(BigMac2003)

View(BigMac2003)

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

# BigMac --------------------------------------------------------------------
  summary(BigMac2003$BigMac)
  cat("Desvio padrão:", sd(BigMac2003$BigMac, na.rm = TRUE), "\n")
  cat("Valores ausentes:", sum(is.na(BigMac2003$BigMac)), "\n")

  # Histograma
  ggplot(BigMac2003) +
   aes(x = BigMac) +
    geom_histogram(colour = "white", fill = "#51A5C5", binwidth = 1) +
    labs(
     x = "Big Mac", 
     y = "Frequência Absoluta",
     title = "Histograma da variável Big Mac"
    ) +
    theme_estat()+
    theme(plot.title = element_text(hjust = 0.5))
  
  #Boxplot
  ggplot(BigMac2003) +
    aes(x = factor(""), y = BigMac) +
    geom_boxplot(fill = "#51A5C5", width = 0.3) + 
    guides(fill = FALSE) +
    stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "white") +
    labs(
      x = "", 
      y = "Big Mac"
    ) +
    theme_estat() +
    theme(plot.title = element_text(hjust = 0.5))

  ggsave("graficos2/box_bigmac.pdf", width = 158, height = 93, units = "mm")

# Bread --------------------------------------------------------------------
  summary(BigMac2003$Bread)
  cat("Desvio padrão:", sd(BigMac2003$Bread, na.rm = TRUE), "\n")
  cat("Valores ausentes:", sum(is.na(BigMac2003$Bread)), "\n")
  
  # Histograma
  ggplot(BigMac2003) +
    aes(x = Bread) +
    geom_histogram(colour = "white", fill = "#51A5C5", binwidth = 1) +
    labs(
      x = "Bread", 
      y = "Frequência Absoluta",
      title = "Histograma da variável Bread"
    ) +
    theme_estat()+
    theme(plot.title = element_text(hjust = 0.5))
  
  #Boxplot
  ggplot(BigMac2003) +
    aes(x = factor(""), y = Bread) +
    geom_boxplot(fill = "#51A5C5", width = 0.3) + 
    guides(fill = FALSE) +
    stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "white") +
    labs(
      x = "", 
      y = "Bread"
    ) +
    theme_estat() +
    theme(plot.title = element_text(hjust = 0.5))
  
  ggsave("graficos2/box_bread.pdf", width = 158, height = 93, units = "mm")

  #Dipersao
  ggplot(BigMac2003, aes(x = Bread, y = BigMac)) +
    geom_point(shape = 16, size = 2) +                     
    geom_smooth(method = "lm", se = FALSE, color = "#51A5C5") +
    labs(
      x = "Bread",
      y = "Big Mac"
    ) +
    meu_tema
  
  ggsave("graficos2/disp_bread.pdf", width = 158, height = 93, units = "mm") 
  
# Rice --------------------------------------------------------------------
  summary(BigMac2003$Rice)
  cat("Desvio padrão:", sd(BigMac2003$Rice, na.rm = TRUE), "\n")
  cat("Valores ausentes:", sum(is.na(BigMac2003$Rice)), "\n")
  
  # Histograma
  ggplot(BigMac2003) +
    aes(x = Rice) +
    geom_histogram(colour = "white", fill = "#51A5C5", binwidth = 1) +
    labs(
      x = "Rice", 
      y = "Frequência Absoluta",
      title = "Histograma da variável Rice"
    ) +
    theme_estat()+
    theme(plot.title = element_text(hjust = 0.5))
  
  #Boxplot
  ggplot(BigMac2003) +
    aes(x = factor(""), y = Rice) +
    geom_boxplot(fill = "#51A5C5", width = 0.3) + 
    guides(fill = FALSE) +
    stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "white") +
    labs(
      x = "", 
      y = "Rice"
    ) +
    theme_estat() +
    theme(plot.title = element_text(hjust = 0.5))
  
  ggsave("graficos2/box_rice.pdf", width = 158, height = 93, units = "mm")
  
  #Dipersao
  ggplot(BigMac2003, aes(x = Rice, y = BigMac)) +
    geom_point(shape = 16, size = 2) +                     
    geom_smooth(method = "lm", se = FALSE, color = "#51A5C5") +
    labs(
      x = "Rice",
      y = "Big Mac"
    ) +
    meu_tema
  
  ggsave("graficos2/disp_rice.pdf", width = 158, height = 93, units = "mm") 
  
# FoodIndex --------------------------------------------------------------------
  summary(BigMac2003$FoodIndex)
  cat("Desvio padrão:", sd(BigMac2003$FoodIndex, na.rm = TRUE), "\n")
  cat("Valores ausentes:", sum(is.na(BigMac2003$FoodIndex)), "\n")
  
  # Histograma
  ggplot(BigMac2003) +
    aes(x = FoodIndex) +
    geom_histogram(colour = "white", fill = "#51A5C5", binwidth = 1) +
    labs(
      x = "Food Index", 
      y = "Frequência Absoluta",
      title = "Histograma da variável Food Index"
    ) +
    theme_estat()+
    theme(plot.title = element_text(hjust = 0.5))
  
  #Boxplot
  ggplot(BigMac2003) +
    aes(x = factor(""), y = FoodIndex) +
    geom_boxplot(fill = "#51A5C5", width = 0.3) + 
    guides(fill = FALSE) +
    stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "white") +
    labs(
      x = "", 
      y = "Food Index"
    ) +
    theme_estat() +
    theme(plot.title = element_text(hjust = 0.5))
  
  ggsave("graficos2/box_FoodIndex.pdf", width = 158, height = 93, units = "mm")
  
  #Dipersao
  ggplot(BigMac2003, aes(x = FoodIndex, y = BigMac)) +
    geom_point(shape = 16, size = 2) +                     
    geom_smooth(method = "lm", se = FALSE, color = "#51A5C5") +
    labs(
      x = "Food Index",
      y = "Big Mac"
    ) +
    meu_tema
  
  ggsave("graficos2/disp_foodindex.pdf", width = 158, height = 93, units = "mm") 
  
# Bus --------------------------------------------------------------------------
  
  summary(BigMac2003$Bus)
  cat("Desvio padrão:", sd(BigMac2003$Bus, na.rm = TRUE), "\n")
  cat("Valores ausentes:", sum(is.na(BigMac2003$Bus)), "\n")
  
  # Histograma
  ggplot(BigMac2003) +
    aes(x = Bus) +
    geom_histogram(colour = "white", fill = "#51A5C5", binwidth = 1) +
    labs(
      x = "Bus", 
      y = "Frequência Absoluta",
      title = "Histograma da variável Bus"
    ) +
    theme_estat()+
    theme(plot.title = element_text(hjust = 0.5))
  
  #Boxplot
  ggplot(BigMac2003) +
    aes(x = factor(""), y = Bus) +
    geom_boxplot(fill = "#51A5C5", width = 0.3) + 
    guides(fill = FALSE) +
    stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "white") +
    labs(
      x = "", 
      y = "Bus"
    ) +
    theme_estat() +
    theme(plot.title = element_text(hjust = 0.5))
  
  ggsave("graficos2/box_bus.pdf", width = 158, height = 93, units = "mm")
  
  #Dipersao
  ggplot(BigMac2003, aes(x = Bus, y = BigMac)) +
    geom_point(shape = 16, size = 2) +                     
    geom_smooth(method = "lm", se = FALSE, color = "#51A5C5") +
    labs(
      x = "Bus",
      y = "Big Mac"
    ) +
    meu_tema
  
  ggsave("graficos2/disp_bus.pdf", width = 158, height = 93, units = "mm") 
  
# Apt --------------------------------------------------------------------------
  
  summary(BigMac2003$Apt)
  cat("Desvio padrão:", sd(BigMac2003$Apt, na.rm = TRUE), "\n")
  cat("Valores ausentes:", sum(is.na(BigMac2003$Apt)), "\n")
  
  # Histograma
  ggplot(BigMac2003) +
    aes(x = Apt) +
    geom_histogram(colour = "white", fill = "#51A5C5") +
    labs(
      x = "Apt", 
      y = "Frequência Absoluta",
      title = "Histograma da variável Apt"
    ) +
    theme_estat()+
    theme(plot.title = element_text(hjust = 0.5))
  
  #Boxplot
  ggplot(BigMac2003) +
    aes(x = factor(""), y = Apt) +
    geom_boxplot(fill = "#51A5C5", width = 0.3) + 
    guides(fill = FALSE) +
    stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "white") +
    labs(
      x = "", 
      y = "Apt"
    ) +
    theme_estat() +
    theme(plot.title = element_text(hjust = 0.5))
  
  ggsave("graficos2/box_apt.pdf", width = 158, height = 93, units = "mm")
  
  #Dipersao
  ggplot(BigMac2003, aes(x = Apt, y = BigMac)) +
    geom_point(shape = 16, size = 2) +                     
    geom_smooth(method = "lm", se = FALSE, color = "#51A5C5") +
    labs(
      x = "Apt",
      y = "Big Mac"
    ) +
    meu_tema
  
  ggsave("graficos2/disp_apt.pdf", width = 158, height = 93, units = "mm") 
  
  
# Teach GI ---------------------------------------------------------------------
  
  summary(BigMac2003$TeachGI)
  cat("Desvio padrão:", sd(BigMac2003$TeachGI, na.rm = TRUE), "\n")
  cat("Valores ausentes:", sum(is.na(BigMac2003$TeachGI)), "\n")
  
  # Histograma
  ggplot(BigMac2003) +
    aes(x = TeachGI) +
    geom_histogram(colour = "white", fill = "#51A5C5") +
    labs(
      x = "Teach GI", 
      y = "Frequência Absoluta",
      title = "Histograma da variável Teach GI"
    ) +
    theme_estat()+
    theme(plot.title = element_text(hjust = 0.5))
  
  #Boxplot
  ggplot(BigMac2003) +
    aes(x = factor(""), y = TeachGI) +
    geom_boxplot(fill = "#51A5C5", width = 0.3) + 
    guides(fill = FALSE) +
    stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "white") +
    labs(
      x = "", 
      y = "Teach GI"
    ) +
    theme_estat() +
    theme(plot.title = element_text(hjust = 0.5))
  
  ggsave("graficos2/box_ateachgi.pdf", width = 158, height = 93, units = "mm")
  
  #Dipersao
  ggplot(BigMac2003, aes(x = TeachGI, y = BigMac)) +
    geom_point(shape = 16, size = 2) +                     
    geom_smooth(method = "lm", se = FALSE, color = "#51A5C5") +
    labs(
      x = "Teach GI",
      y = "Big Mac"
    ) +
    meu_tema
  
  ggsave("graficos2/disp_Teach GI.pdf", width = 158, height = 93, units = "mm") 
  
# Teach NI ---------------------------------------------------------------------
     
  summary(BigMac2003$TeachNI)
  cat("Desvio padrão:", sd(BigMac2003$TeachNI, na.rm = TRUE), "\n")
  cat("Valores ausentes:", sum(is.na(BigMac2003$TeachNI)), "\n")
  
  # Histograma
  ggplot(BigMac2003) +
    aes(x = TeachNI) +
    geom_histogram(colour = "white", fill = "#51A5C5") +
    labs(
      x = "Teach NI", 
      y = "Frequência Absoluta",
      title = "Histograma da variável Teach NI"
    ) +
    theme_estat()+
    theme(plot.title = element_text(hjust = 0.5))
  
  #Boxplot
  ggplot(BigMac2003) +
    aes(x = factor(""), y = TeachNI) +
    geom_boxplot(fill = "#51A5C5", width = 0.3) + 
    guides(fill = FALSE) +
    stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "white") +
    labs(
      x = "", 
      y = "Teach NI"
    ) +
    theme_estat() +
    theme(plot.title = element_text(hjust = 0.5))
  
  ggsave("graficos2/box_teachni.pdf", width = 158, height = 93, units = "mm")
  
  #Dipersao
  ggplot(BigMac2003, aes(x = TeachNI, y = BigMac)) +
    geom_point(shape = 16, size = 2) +                     
    geom_smooth(method = "lm", se = FALSE, color = "#51A5C5") +
    labs(
      x = "Teach NI",
      y = "Big Mac"
    ) +
    meu_tema
  
  ggsave("graficos2/disp_Teach NI.pdf", width = 158, height = 93, units = "mm") 
  
# Tax Rate ---------------------------------------------------------------------
  
  summary(BigMac2003$TaxRate)
  cat("Desvio padrão:", sd(BigMac2003$TaxRate, na.rm = TRUE), "\n")
  cat("Valores ausentes:", sum(is.na(BigMac2003$TaxRate)), "\n")
  
  # Histograma
  ggplot(BigMac2003) +
    aes(x = TaxRate) +
    geom_histogram(colour = "white", fill = "#51A5C5") +
    labs(
      x = "Tax Rate", 
      y = "Frequência Absoluta",
      title = "Histograma da variável Tax Rate"
    ) +
    theme_estat()+
    theme(plot.title = element_text(hjust = 0.5))
  
  #Boxplot
  ggplot(BigMac2003) +
    aes(x = factor(""), y = TaxRate) +
    geom_boxplot(fill = "#51A5C5", width = 0.3) + 
    guides(fill = FALSE) +
    stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "white") +
    labs(
      x = "", 
      y = "Tax Rate"
    ) +
    theme_estat() +
    theme(plot.title = element_text(hjust = 0.5))
  
  ggsave("graficos2/box_taxrate.pdf", width = 158, height = 93, units = "mm")
  
  #Dipersao
  ggplot(BigMac2003, aes(x = TaxRate, y = BigMac)) +
    geom_point(shape = 16, size = 2) +                     
    geom_smooth(method = "lm", se = FALSE, color = "#51A5C5") +
    labs(
      x = "Tax Rate",
      y = "Big Mac"
    ) +
    meu_tema
  
  ggsave("graficos2/disp_taxrate.pdf", width = 158, height = 93, units = "mm") 
  
# Teach Hours ------------------------------------------------------------------
  
  summary(BigMac2003$TeachHours)
  cat("Desvio padrão:", sd(BigMac2003$TeachHours, na.rm = TRUE), "\n")
  cat("Valores ausentes:", sum(is.na(BigMac2003$TeachHours)), "\n")
  
  # Histograma
  ggplot(BigMac2003) +
    aes(x = TeachHours) +
    geom_histogram(colour = "white", fill = "#51A5C5") +
    labs(
      x = "Teach Hours", 
      y = "Frequência Absoluta",
      title = "Histograma da variável Teach Hours"
    ) +
    theme_estat()+
    theme(plot.title = element_text(hjust = 0.5))
  
  #Boxplot
  ggplot(BigMac2003) +
    aes(x = factor(""), y = TeachHours) +
    geom_boxplot(fill = "#51A5C5", width = 0.3) + 
    guides(fill = FALSE) +
    stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "white") +
    labs(
      x = "", 
      y = "Teach Hours"
    ) +
    theme_estat() +
    theme(plot.title = element_text(hjust = 0.5))
  
  ggsave("graficos2/box_teachhours.pdf", width = 158, height = 93, units = "mm")
  
  #Dipersao
  ggplot(BigMac2003, aes(x = TeachHours, y = BigMac)) +
    geom_point(shape = 16, size = 2) +                     
    geom_smooth(method = "lm", se = FALSE, color = "#51A5C5") +
    labs(
      x = "Teach Hours",
      y = "BigMac"
    ) +
    meu_tema
  
  ggsave("graficos2/disp_teachhours.pdf", width = 158, height = 93, units = "mm") 
  
  
library(reshape2)
library(RColorBrewer)
corr <- cor(BigMac2003)
corr_melt <- melt(corr)

ggplot(corr_melt, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(label = sprintf("%.2f", value))) +
  labs(x = NULL, y = NULL) + 
  scale_fill_distiller(palette = "RdYlBu") +
  meu_tema +
  theme(
    axis.text.x = element_text(size = 7.8),  # tamanho menor eixo X
    axis.text.y = element_text(size = 7.8),  # tamanho menor eixo Y
    axis.title.x = element_blank(), 
    axis.title.y = element_blank(),
    panel.grid = element_blank()
  ) +
  coord_fixed()

