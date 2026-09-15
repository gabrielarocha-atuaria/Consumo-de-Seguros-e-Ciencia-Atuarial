# ============================================================
# V08 — NÍVEL EDUCACIONAL DO CHEFE DA FAMÍLIA
# ============================================================
# Etapa: Tratamento categórico ordinal / análise descritiva
#
# OBJETIVO:
# Caracterizar o nível educacional do chefe da família dos
# respondentes, preservando a ordenação das categorias de
# escolaridade observadas na amostra.
#
# IMPORTANTE:
# A variável possui natureza ordinal. Portanto, suas categorias
# são organizadas segundo a progressão dos níveis de escolaridade,
# sem atribuição de distâncias numéricas entre elas.
# ============================================================


# ------------------------------------------------------------
# 1. SELEÇÃO DA VARIÁVEL
# ------------------------------------------------------------

df_escolaridade <- df %>%
  select(NivelEducacionaldoChefe)


# ------------------------------------------------------------
# 2. LIMPEZA TEXTUAL
# ------------------------------------------------------------

# Remove espaços excedentes e garante que a variável seja
# inicialmente tratada como texto antes da conversão para fator.

df_escolaridade <- df_escolaridade %>%
  mutate(
    NivelEducacionaldoChefe = trimws(
      as.character(NivelEducacionaldoChefe)
    )
  )


# ------------------------------------------------------------
# 3. INSPEÇÃO DAS CATEGORIAS ORIGINAIS
# ------------------------------------------------------------

unique(
  df_escolaridade$NivelEducacionaldoChefe
)

table(
  df_escolaridade$NivelEducacionaldoChefe,
  useNA = "ifany"
)


# ------------------------------------------------------------
# 4. DEFINIÇÃO DA ORDEM EDUCACIONAL
# ------------------------------------------------------------

# A ordem segue a progressão dos níveis de escolaridade
# identificados no questionário.
#
# "Não sei informar" é mantido como categoria de resposta,
# mas não representa propriamente um nível educacional.

ordem_educacao <- c(
  "Fundamental incompleto",
  "Fundamental completo",
  "Médio incompleto",
  "Médio completo",
  "Superior incompleto",
  "Superior completo",
  "Não sei informar"
)


# ------------------------------------------------------------
# 5. CONVERSÃO PARA VARIÁVEL ORDINAL
# ------------------------------------------------------------

df_escolaridade <- df_escolaridade %>%
  mutate(
    NivelEducacionaldoChefe = factor(
      NivelEducacionaldoChefe,
      levels = ordem_educacao,
      ordered = TRUE
    )
  )


# ------------------------------------------------------------
# 6. CONFERÊNCIA APÓS A CONVERSÃO
# ------------------------------------------------------------

levels(
  df_escolaridade$NivelEducacionaldoChefe
)

table(
  df_escolaridade$NivelEducacionaldoChefe,
  useNA = "ifany"
)


# ------------------------------------------------------------
# 7. CONTROLE DE OBSERVAÇÕES
# ------------------------------------------------------------

n_validos_escolaridade <- sum(
  !is.na(
    df_escolaridade$NivelEducacionaldoChefe
  )
)

n_ausentes_escolaridade <- sum(
  is.na(
    df_escolaridade$NivelEducacionaldoChefe
  )
)

n_validos_escolaridade
n_ausentes_escolaridade


# ------------------------------------------------------------
# 8. DISTRIBUIÇÃO DE FREQUÊNCIAS
# ------------------------------------------------------------

# A tabela mantém a ordenação natural das categorias.

tabela_escolaridade <- df_escolaridade %>%
  filter(
    !is.na(NivelEducacionaldoChefe)
  ) %>%
  count(
    NivelEducacionaldoChefe,
    name = "Frequencia",
    .drop = FALSE
  ) %>%
  mutate(
    Porcentagem =
      Frequencia / sum(Frequencia) * 100
  )

print(tabela_escolaridade)


# ------------------------------------------------------------
# 9. VISUALIZAÇÃO DA DISTRIBUIÇÃO
# ------------------------------------------------------------

grafico_escolaridade <- ggplot(
  df_escolaridade %>%
    filter(
      !is.na(NivelEducacionaldoChefe)
    ),
  aes(
    x = NivelEducacionaldoChefe
  )
) +
  geom_bar() +
  geom_text(
    stat = "count",
    aes(
      label = after_stat(count)
    ),
    vjust = -0.5,
    size = 3.5
  ) +
  scale_y_continuous(
    expand = expansion(
      mult = c(0, 0.10)
    )
  ) +
  labs(
    title = "Nível Educacional do Chefe da Família",
    subtitle = paste0(
      "Respondentes válidos: n = ",
      n_validos_escolaridade
    ),
    x = "Grau de Escolaridade",
    y = "Frequência (n)",
    caption = "Fonte: Elaborado pela autora (2026)."
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(
      angle = 60,
      hjust = 1,
      vjust = 1
    )
  )

grafico_escolaridade


# ------------------------------------------------------------
# FIM — V08 NÍVEL EDUCACIONAL DO CHEFE DA FAMÍLIA
# ------------------------------------------------------------
