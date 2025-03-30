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

spec(dados_nao_tratados)

# Carregando a tabela bruta
dados_nao_tratados <- read_csv("dados_nao_tratados.csv", quote = "\"", locale = locale(encoding = "UTF-8"))

# Separar os dados da coluna Time em múltiplas colunas
dados_apos_split_time <- dados_nao_tratados %>%
  separate(Time, into = c("Equipe", "Numero", "Posicao"), sep = "\\|", remove = FALSE) %>%
  mutate(Equipe = trimws(Equipe),  # Remover espaços antes e depois
         Posicao = trimws(Posicao))  # Remover espaços antes e depois
  
# Ajuste valores não convertidos
dados_apos_split_time_ajustado <- dados_apos_split_time %>%
  mutate(Posicao = ifelse(row_number() %in% c(59, 413, 477, 508), Numero, Posicao))

head(dados_apos_split_time_ajustado, 10)

#Verificando
valores_especificos <- dados_apos_split_time_ajustado %>%
  slice(c(59, 413, 477, 508)) %>%  # Seleciona as linhas
  select(Equipe, Numero, Posicao)  # Seleciona apenas as colunas desejadas

valores_especificos

# Excluir colunas
dados_apos_exclusao_colunas <- dados_apos_split_time_ajustado %>% select(-Links, -Time, -Numero)

head(dados_apos_exclusao_colunas, 10)

