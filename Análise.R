install.packages("renv")
library(renv)
renv::init()

renv::snapshot()
renv::restore()

# Instalação de pacotes

install.packages('tidyverse')
install.packages('ggplot2')
install.packages('summarytools')
install.packages('dlookr')
install.packages('readxl')
install.packages('knitr')
install.packages('scales')
install.packages('dplyr')

library(tidyverse)
library(ggplot2)
library(summarytools)
library(dlookr)
library(readxl)
library(knitr)
library(scales)
library(dplyr)
library(readr)
library(tidyr)
library(stringr)
library(lubridate)

#
# Carregando a tabela bruta
dados_tratados <- read_csv("dados_apos_coluna_mes.csv", quote = "\"", locale = locale(encoding = "UTF-8"))

# Transformando peso, altura, ppg, rpg e apg em númerico
dados_tratados <- dados_tratados %>%
  mutate(
    PPG = as.numeric(PPG),
    RPG = as.numeric(RPG),
    APG = as.numeric(APG),
    Altura = as.numeric(Altura),
    Peso = as.numeric(Peso)
  )
