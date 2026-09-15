# ============================================================
# PREPARAÇÃO DA BASE DE DADOS
# ============================================================
#
# Objetivo:
# Importar, inspecionar e estruturar os dados provenientes do
# questionário antes dos tratamentos específicos de cada variável.
#
# A base original de respostas não é disponibilizada neste
# repositório, a fim de preservar os dados dos participantes.
# ============================================================


# ------------------------------------------------------------
# 1. CARREGAMENTO DOS PACOTES
# ------------------------------------------------------------

library(readxl)
library(tidyverse)
library(janitor)


# ------------------------------------------------------------
# 2. DEFINIÇÃO DO ARQUIVO DE ENTRADA
# ------------------------------------------------------------

# O script utiliza caminho relativo ao diretório do projeto,
# evitando caminhos absolutos específicos do computador do autor.

arquivo_dados <- "dados/Pesquisa_aplicada_TCC2.xlsx"


# ------------------------------------------------------------
# 3. IMPORTAÇÃO DA BASE
# ------------------------------------------------------------
# Arquivo em Excel

df <- read_excel(arquivo_dados)


# ------------------------------------------------------------
# 4. INSPEÇÃO INICIAL
# ------------------------------------------------------------

# Dimensão da base: número de observações × número de colunas
dim(df)

# Estrutura e tipos das variáveis importadas
str(df)

# Primeiras observações
head(df)

# Visão compacta da estrutura da base
glimpse(df)

# Estatísticas descritivas iniciais
summary(df)


# ------------------------------------------------------------
# 5. REMOÇÃO DE COLUNAS ADMINISTRATIVAS
# ------------------------------------------------------------

# As duas primeiras colunas provenientes do formulário não
# integram as variáveis utilizadas no modelo analítico.

df <- df %>%
  select(-1, -2)


# ------------------------------------------------------------
# 6. PADRONIZAÇÃO DOS NOMES DAS COLUNAS
# ------------------------------------------------------------

# Padroniza os nomes provenientes do questionário para facilitar
# sua manipulação no R.

df <- df %>%
  clean_names()


# ------------------------------------------------------------
# 7. CONFERÊNCIA APÓS A PADRONIZAÇÃO
# ------------------------------------------------------------

names(df)
dim(df)
glimpse(df)
