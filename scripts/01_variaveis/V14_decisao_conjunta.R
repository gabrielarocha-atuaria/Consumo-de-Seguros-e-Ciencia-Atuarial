# ============================================================
# V14 — DECISÃO CONJUNTA
# ============================================================
# Etapa: Tratamento categórico / análise descritiva
#
# OBJETIVO:
# Caracterizar se a decisão de contratar um seguro é discutida
# em conjunto pela família dos respondentes.
#
# IMPORTANTE:
# A variável é tratada como categórica nominal. As categorias
# representam respostas distintas à pergunta do questionário
# e não possuem ordenação quantitativa.
# ============================================================


# ------------------------------------------------------------
# 1. SELEÇÃO DA VARIÁVEL
# ------------------------------------------------------------

df_decisao <- df %>%
  transmute(
    DecisaoConjunta =
      a_decisao_de_contratar_um_seguro_e_discutida_em_conjunto_pela_familia
  )


# ------------------------------------------------------------
# 2. INSPEÇÃO DAS RESPOSTAS ORIGINAIS
# ------------------------------------------------------------

head(
  df_decisao$DecisaoConjunta
)

unique(
  df_decisao$DecisaoConjunta
)

table(
  df_decisao$DecisaoConjunta,
  useNA = "ifany"
)


# ------------------------------------------------------------
# 3. CONVERSÃO PARA VARIÁVEL CATEGÓRICA
# ------------------------------------------------------------

# A conversão para fator preserva as categorias efetivamente
# observadas na base, sem atribuir ordenação entre elas.

df_decisao <- df_decisao %>%
  mutate(
    DecisaoConjunta = factor(
      DecisaoConjunta
    )
  )


# ------------------------------------------------------------
# 4. CONFERÊNCIA APÓS A CONVERSÃO
# ------------------------------------------------------------

levels(
  df_decisao$DecisaoConjunta
)

table(
  df_decisao$DecisaoConjunta,
  useNA = "ifany"
)


# ------------------------------------------------------------
# 5. CONTROLE DE OBSERVAÇÕES
# ------------------------------------------------------------

n_validos_decisao <- sum(
  !is.na(
    df_decisao$DecisaoConjunta
  )
)

n_ausentes_decisao <- sum(
  is.na(
    df_decisao$DecisaoConjunta
  )
)

n_validos_decisao
n_ausentes_decisao


# ------------------------------------------------------------
# 6. DISTRIBUIÇÃO DE FREQUÊNCIAS
# ------------------------------------------------------------

tabela_decisao <- df_decisao %>%
  filter(
    !is.na(DecisaoConjunta)
  ) %>%
  count(
    DecisaoConjunta,
    name = "Frequencia"
  ) %>%
  mutate(
    Porcentagem =
      Frequencia / sum(Frequencia) * 100
  ) %>%
  arrange(
    desc(Frequencia)
  )

print(tabela_decisao)


# ------------------------------------------------------------
# 7. VISUALIZAÇÃO DA DISTRIBUIÇÃO
# ------------------------------------------------------------

grafico_decisao <- ggplot(
  tabela_decisao,
  aes(
    x = reorder(
      DecisaoConjunta,
      -Frequencia
    ),
    y = Frequencia
  )
) +
  geom_col(
    width = 0.65
  ) +
  geom_text(
    aes(
      label = paste0(
        Frequencia,
        " (",
        round(Porcentagem, 1),
        "%)"
      )
    ),
    vjust = -0.5,
    fontface = "bold"
  ) +
  scale_y_continuous(
    expand = expansion(
      mult = c(0, 0.12)
    )
  ) +
  labs(
    title = "Decisão Familiar sobre a Contratação de Seguros",
    subtitle = paste0(
      "Respondentes válidos: n = ",
      n_validos_decisao
    ),
    x = "A decisão é discutida em conjunto?",
    y = "Frequência Absoluta (n)",
    caption = "Fonte: Elaborado pela autora (2026)."
  ) +
  theme_minimal()

grafico_decisao


# ------------------------------------------------------------
# FIM — V14 DECISÃO CONJUNTA
# ------------------------------------------------------------
