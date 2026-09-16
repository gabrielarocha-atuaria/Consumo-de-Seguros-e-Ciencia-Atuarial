# ============================================================
# V11 — CONHECIMENTO FINANCEIRO
# ============================================================
# Etapa: Tratamento categórico / análise descritiva
#
# OBJETIVO:
# Caracterizar a declaração dos respondentes quanto à posse
# de conhecimentos sobre educação financeira e finanças
# pessoais.
#
# IMPORTANTE:
# A variável é categórica nominal dicotômica, composta pelas
# respostas "Sim" e "Não".
# ============================================================


# ------------------------------------------------------------
# 1. SELEÇÃO DA VARIÁVEL
# ------------------------------------------------------------

df_conhecimento <- df %>%
  transmute(
    ConhecimentoFinanceiro =
      o_a_entrevistado_a_possui_conhecimentos_sobre_educacao_financeira_e_financas_pessoais
  )


# ------------------------------------------------------------
# 2. INSPEÇÃO DAS RESPOSTAS ORIGINAIS
# ------------------------------------------------------------

unique(
  df_conhecimento$ConhecimentoFinanceiro
)

table(
  df_conhecimento$ConhecimentoFinanceiro,
  useNA = "ifany"
)


# ------------------------------------------------------------
# 3. PADRONIZAÇÃO DA VARIÁVEL
# ------------------------------------------------------------

# As categorias observadas no questionário são explicitamente
# definidas para evitar a criação acidental de outros níveis.

df_conhecimento <- df_conhecimento %>%
  mutate(
    ConhecimentoFinanceiro = factor(
      ConhecimentoFinanceiro,
      levels = c(
        "Sim",
        "Não"
      )
    )
  )


# ------------------------------------------------------------
# 4. CONFERÊNCIA APÓS A CONVERSÃO
# ------------------------------------------------------------

levels(
  df_conhecimento$ConhecimentoFinanceiro
)

table(
  df_conhecimento$ConhecimentoFinanceiro,
  useNA = "ifany"
)


# ------------------------------------------------------------
# 5. CONTROLE DE OBSERVAÇÕES
# ------------------------------------------------------------

n_validos_conhecimento <- sum(
  !is.na(
    df_conhecimento$ConhecimentoFinanceiro
  )
)

n_ausentes_conhecimento <- sum(
  is.na(
    df_conhecimento$ConhecimentoFinanceiro
  )
)

n_validos_conhecimento
n_ausentes_conhecimento


# ------------------------------------------------------------
# 6. DISTRIBUIÇÃO DE FREQUÊNCIAS
# ------------------------------------------------------------

tabela_conhecimento <- df_conhecimento %>%
  filter(
    !is.na(ConhecimentoFinanceiro)
  ) %>%
  count(
    ConhecimentoFinanceiro,
    name = "Frequencia",
    .drop = FALSE
  ) %>%
  mutate(
    Porcentagem =
      Frequencia / sum(Frequencia) * 100
  )

print(tabela_conhecimento)


# ------------------------------------------------------------
# 7. VISUALIZAÇÃO DA DISTRIBUIÇÃO
# ------------------------------------------------------------

grafico_conhecimento <- ggplot(
  tabela_conhecimento,
  aes(
    x = ConhecimentoFinanceiro,
    y = Frequencia
  )
) +
  geom_col(
    width = 0.6
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
    title = "Conhecimento sobre Educação Financeira e Finanças Pessoais",
    subtitle = paste0(
      "Respondentes válidos: n = ",
      n_validos_conhecimento
    ),
    x = "Resposta",
    y = "Frequência Absoluta (n)",
    caption = "Fonte: Elaborado pela autora (2026)."
  ) +
  theme_minimal()

grafico_conhecimento


# ------------------------------------------------------------
# FIM — V11 CONHECIMENTO FINANCEIRO
# ------------------------------------------------------------
