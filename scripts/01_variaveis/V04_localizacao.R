# ============================================================
# V04 — LOCALIZAÇÃO DO(A) RESPONDENTE
# ============================================================
# Etapa: Tratamento textual / análise descritiva
#
# OBJETIVO:
# Padronizar as informações de localização declaradas pelos
# respondentes e descrever sua distribuição territorial.
#
# IMPORTANTE:
# A padronização busca reduzir diferenças de grafia, acentuação,
# abreviações e pontuação sem alterar a informação territorial
# identificável na resposta original.
# ============================================================


# ------------------------------------------------------------
# 1. SELEÇÃO DA VARIÁVEL
# ------------------------------------------------------------

df_localizacao <- df %>%
  select(Localizacao)


# ------------------------------------------------------------
# 2. INSPEÇÃO DAS RESPOSTAS ORIGINAIS
# ------------------------------------------------------------

# Como a localização foi coletada por resposta aberta, a inspeção
# inicial permite identificar diferentes formas de registrar
# uma mesma cidade ou unidade federativa.

unique(df_localizacao$Localizacao)

table(
  df_localizacao$Localizacao,
  useNA = "ifany"
)


# ------------------------------------------------------------
# 3. NORMALIZAÇÃO TEXTUAL
# ------------------------------------------------------------

# É criada uma variável auxiliar para tornar as comparações
# textuais mais consistentes.
#
# A normalização:
# - remove diferenças de acentuação;
# - converte o texto para caixa alta;
# - remove espaços excedentes;
# - remove sinais de pontuação.
#
# A resposta original permanece preservada em Localizacao.

df_localizacao <- df_localizacao %>%
  mutate(
    loc_norm = stringi::stri_trans_general(
      Localizacao,
      "Latin-ASCII"
    ),
    loc_norm = stringr::str_to_upper(loc_norm),
    loc_norm = stringr::str_trim(loc_norm),
    loc_limpa = stringr::str_remove_all(
      loc_norm,
      "[[:punct:]]"
    )
  )


# ------------------------------------------------------------
# 4. PADRONIZAÇÃO DAS CIDADES
# ------------------------------------------------------------

# Diferentes formas de escrita que permitem identificar a mesma
# cidade são reunidas sob uma denominação padronizada.
#
# O tratamento é explícito no código para permitir a conferência
# das regras aplicadas às respostas.

df_localizacao <- df_localizacao %>%
  mutate(
    cidade_final = case_when(

      str_detect(
        loc_limpa,
        "JOAO PESSOA|JOAOSSOA|J PESS"
      ) ~ "João Pessoa",

      str_detect(loc_limpa, "BAYEUX") ~ "Bayeux",

      str_detect(
        loc_limpa,
        "SANTA RITA"
      ) ~ "Santa Rita",

      str_detect(
        loc_limpa,
        "CABEDELO"
      ) ~ "Cabedelo",

      str_detect(
        loc_limpa,
        "CAMPINA GRANDE"
      ) ~ "Campina Grande",

      str_detect(
        loc_limpa,
        "PEDRAS DE FOGO"
      ) ~ "Pedras de Fogo",

      str_detect(
        loc_limpa,
        "ALAGOINHA"
      ) ~ "Alagoinha",

      str_detect(
        loc_limpa,
        "ALHANDRA"
      ) ~ "Alhandra",

      str_detect(
        loc_limpa,
        "CALDAS BRANDAO"
      ) ~ "Caldas Brandão",

      str_detect(
        loc_limpa,
        "ITAPOROROCA"
      ) ~ "Itapororoca",

      str_detect(
        loc_limpa,
        "LUCENA"
      ) ~ "Lucena",

      str_detect(loc_limpa, "SAPE") ~ "Sapé",

      str_detect(loc_limpa, "CONDE") ~ "Conde",

      str_detect(
        loc_limpa,
        "ITABAIANA"
      ) ~ "Itabaiana",

      str_detect(loc_limpa, "RECIFE") ~ "Recife",

      str_detect(loc_limpa, "GOIANA") ~ "Goiana",

      str_detect(loc_limpa, "MARINGA") ~ "Maringá",

      str_detect(
        loc_limpa,
        "SAO PAULO"
      ) ~ "São Paulo",

      # Resposta contendo apenas a unidade federativa.
      loc_limpa == "PARAIBA" ~ "Paraiba",

      # Mantém as demais respostas normalizadas para conferência.
      TRUE ~ str_to_title(loc_norm)
    )
  )


# ------------------------------------------------------------
# 5. CLASSIFICAÇÃO POR UNIDADE FEDERATIVA
# ------------------------------------------------------------

# Após a padronização das cidades, as observações são classificadas
# segundo a unidade federativa correspondente.
#
# Respostas que não podem ser classificadas pelas regras disponíveis
# permanecem identificadas como "Não Informado" para posterior
# conferência, em vez de receber uma classificação presumida.

df_localizacao <- df_localizacao %>%
  mutate(
    estado_final = case_when(

      cidade_final %in% c(
        "João Pessoa",
        "Bayeux",
        "Santa Rita",
        "Cabedelo",
        "Campina Grande",
        "Conde",
        "Sapé",
        "Itabaiana",
        "Alagoinha",
        "Alhandra",
        "Pedras de Fogo",
        "Itapororoca",
        "Caldas Brandão",
        "Lucena",
        "Paraiba"
      ) ~ "Paraíba",

      cidade_final %in% c(
        "Recife",
        "Goiana"
      ) ~ "Pernambuco",

      cidade_final == "Maringá" ~ "Paraná",

      cidade_final == "São Paulo" ~ "São Paulo",

      TRUE ~ "Não Informado"
    )
  )


# ------------------------------------------------------------
# 6. CONFERÊNCIA DO TRATAMENTO
# ------------------------------------------------------------

# A tabela permite conferir simultaneamente a cidade padronizada
# e a unidade federativa atribuída.
#
# Essa etapa é especialmente importante em variáveis originadas
# de respostas abertas.

tabela_conferencia_localizacao <- df_localizacao %>%
  count(
    estado_final,
    cidade_final,
    sort = TRUE
  )

print(
  tabela_conferencia_localizacao,
  n = Inf
)


# ------------------------------------------------------------
# 7. DISTRIBUIÇÃO POR UNIDADE FEDERATIVA
# ------------------------------------------------------------

tabela_uf <- df_localizacao %>%
  count(
    estado_final,
    name = "Frequencia"
  ) %>%
  mutate(
    Porcentagem = Frequencia /
      sum(Frequencia) * 100
  ) %>%
  arrange(desc(Frequencia))

print(tabela_uf)


# ------------------------------------------------------------
# 8. DISTRIBUIÇÃO POR CIDADE
# ------------------------------------------------------------

tabela_cidade <- df_localizacao %>%
  count(
    cidade_final,
    name = "Frequencia"
  ) %>%
  arrange(desc(Frequencia))

print(tabela_cidade)


# ------------------------------------------------------------
# 9. VISUALIZAÇÃO DA DISTRIBUIÇÃO TERRITORIAL
# ------------------------------------------------------------

# O gráfico apresenta a distribuição das observações após a
# padronização territorial.
#
# A visualização possui finalidade descritiva e não implica
# representatividade estatística das localidades apresentadas.

grafico_localizacao <- tabela_uf %>%
  ggplot(
    aes(
      x = reorder(
        estado_final,
        Frequencia
      ),
      y = Frequencia
    )
  ) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Distribuição dos Respondentes por Unidade Federativa",
    x = "Unidade Federativa",
    y = "Frequência",
    caption = "Fonte: Elaborado pela autora (2026)."
  ) +
  theme_minimal()

grafico_localizacao


# ------------------------------------------------------------
# FIM — V04 LOCALIZAÇÃO
# ------------------------------------------------------------
