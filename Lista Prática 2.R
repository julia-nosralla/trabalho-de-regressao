# LISTA PRÁTICA 2 DE ANÁLISE DE REGRESSÃO LINEAR - 2025.1

setwd("C:/Users/marin/Documents/YAY/Regressão/Lista Prática 2")

library(readr)
library(tidyverse)
require(car)
require(MASS)
require(lmtest)
require(glmtoolbox)

FUEL = read_delim("fuel2001.csv", delim = ";", 
                  escape_double = FALSE, trim_ws = TRUE)
View(FUEL)
dados = FUEL
attach(dados)


# Variáveis -----------------------------------------------------------------------------------------------------

# UF: unidade da federacao;
# Drivers: numero de motoristas licenciados;
# FuelC: total de gasolina vendida (em mil galoes);
# Income: renda per capita em 2000 (em mil USD); 
# Miles: total de milhas em estradas federais;
# Pop: populacao ≥ 16 anos;
# Tax: taxa da gasolina (em cents por galao);
#

#  REGION, COUNTRY e POP não serão usados
