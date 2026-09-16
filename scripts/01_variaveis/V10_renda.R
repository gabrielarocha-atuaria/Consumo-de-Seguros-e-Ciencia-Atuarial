# ============================================================
# V10 — RENDA FAMILIAR
# ============================================================
# Etapa: Tratamento categórico ordinal / análise descritiva
#
# OBJETIVO:
# Caracterizar a distribuição da renda familiar dos respondentes
# segundo as faixas de salário mínimo utilizadas no questionário.
#
# IMPORTANTE:
# A variável renda é tratada como categórica ordinal. A ordenação
# representa a progressão das faixas de renda, sem pressupor
# distâncias iguais entre as categorias.
# ============================================================


# ------------------------------------------------------------
# 1. SELEÇÃO DA VARIÁVEL
# ------------------------------------------------------------

df_renda <- df %>%
  select(renda)


# ------------------------------------------------------------
# 2. INSPEÇÃO DAS CATEGORIAS ORIGINAIS
# ------------------------------------------------------------

unique(df_renda$renda)

table(
  df_renda$renda,
  useNA = "ifany"
)


# ------------------------------------------------------------
# 3. DEFINIÇÃO DA ORDEM DAS FAIXAS DE RENDA
# ------------------------------------------------------------

ordem_renda <- c(
  "Até 1 Salário Mínimo (SM)",
  "Entre 1 e 3 SM",
  "Entre 3 e 5 SM",
  "Entre 5 e 7 SM",
  "Acima de 7 SM"
)


# ------------------------------------------------------------
# 4. CONVERSÃO PARA VARIÁVEL ORDINAL
# ------------------------------------------------------------

df_renda <- df_renda %>%
  mutate(
    renda = factor(
      renda,
      levels = ordem_renda,
      ordered = TRUE
    )
  )


# ------------------------------------------------------------
# 5. CONFERÊNCIA APÓS A CONVERSÃO
# ------------------------------------------------------------

levels(df_renda$renda)

table(
  df_renda$renda,
  useNA = "ifany"
)


# ------------------------------------------------------------
# 6. CONTROLE DE OBSERVAÇÕES
# ------------------------------------------------------------

n_validos_renda <- sum(
  !is.na(df_renda$renda)
)

n_ausentes_renda <- sum(
  is.na(df_renda$renda)
)

n_validos_renda
n_ausentes_renda


# ------------------------------------------------------------
# 7. DISTRIBUIÇÃO DE FREQUÊNCIAS
# ------------------------------------------------------------

# Frequências e percentuais são calculados diretamente da base,
# evitando a inserção manual dos resultados.

tabela_renda <- df_renda %>%
  filter(
    !is.na(renda)
  ) %>%
  count(
    renda,
    name = "Frequencia",
    .drop = FALSE
  ) %>%
  mutate(
    Porcentagem =
      Frequencia / sum(Frequencia) * 100,
    Porcentagem_Acumulada =
      cumsum(Porcentagem)
  )

print(tabela_renda)


# ------------------------------------------------------------
# 8. MEDIANA DA CATEGORIA ORDINAL
# ------------------------------------------------------------

# Para localizar a categoria mediana, os níveis ordenados são
# temporariamente representados por posições de 1 a 5.
#
# O número obtido representa a posição da categoria na escala,
# e não um valor monetário de renda.

renda_posicao <- as.numeric(
  df_renda$renda
)

mediana_posicao_renda <- median(
  renda_posicao,
  na.rm = TRUE
)

mediana_categoria_renda <- ordem_renda[
  mediana_posicao_renda
]

mediana_posicao_renda
mediana_categoria_renda


# ------------------------------------------------------------
# 9. VISUALIZAÇÃO DA DISTRIBUIÇÃO
# ------------------------------------------------------------

grafico_renda <- ggplot(
  tabela_renda,
  aes(
    x = renda,
    y = Frequencia
  )
) +
  geom_col(
    width = 0.8
  ) +
  geom_text(
    aes(
      label = paste0(
        "n = ",
        Frequencia,
        " (",
        round(Porcentagem, 1),
        "%)"
      )
    ),
    hjust = -0.1,
    size = 3.5
  ) +
  coord_flip() +
  scale_y_continuous(
    expand = expansion(
      mult = c(0, 0.20)
    )
  ) +
  labs(
    title = "Distribuição da Renda Familiar",
    subtitle = paste0(
      "Respondentes válidos: n = ",
      n_validos_renda
    ),
    x = "Faixa de Renda",
    y = "Frequência Absoluta (n)",
    caption = "Fonte: Elaborado pela autora (2026)."
  ) +
  theme_minimal()

grafico_renda


# ------------------------------------------------------------
# FIM — V10 RENDA FAMILIAR
# ------------------------------------------------------------
