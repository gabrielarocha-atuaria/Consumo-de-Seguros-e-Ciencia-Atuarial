# ============================================================
# V12 — PERCEPÇÃO DE RISCO
# ============================================================
# Etapa: Tratamento categórico ordinal / análise descritiva
#
# OBJETIVO:
# Caracterizar a percepção de risco declarada pelos respondentes,
# considerando as categorias utilizadas no questionário.
#
# IMPORTANTE:
# A variável é tratada como categórica ordinal, seguindo a
# ordenação: Avesso < Moderado < Propenso.
#
# Essa ordenação representa a posição das categorias na escala,
# sem pressupor distâncias quantitativas iguais entre elas.
# ============================================================


# ------------------------------------------------------------
# 1. SELEÇÃO DA VARIÁVEL
# ------------------------------------------------------------

df_risco <- df %>%
  transmute(
    PercepcaodeRisco =
      qual_e_a_percepcao_de_risco_do_a_entrevistado_a
  )


# ------------------------------------------------------------
# 2. INSPEÇÃO DAS RESPOSTAS ORIGINAIS
# ------------------------------------------------------------

head(
  df_risco$PercepcaodeRisco
)

unique(
  df_risco$PercepcaodeRisco
)

table(
  df_risco$PercepcaodeRisco,
  useNA = "ifany"
)


# ------------------------------------------------------------
# 3. DEFINIÇÃO DA ORDEM DAS CATEGORIAS
# ------------------------------------------------------------

ordem_risco <- c(
  "Avesso",
  "Moderado",
  "Propenso"
)


# ------------------------------------------------------------
# 4. CONVERSÃO PARA VARIÁVEL ORDINAL
# ------------------------------------------------------------

df_risco <- df_risco %>%
  mutate(
    PercepcaodeRisco = factor(
      PercepcaodeRisco,
      levels = ordem_risco,
      ordered = TRUE
    )
  )


# ------------------------------------------------------------
# 5. CONFERÊNCIA APÓS A CONVERSÃO
# ------------------------------------------------------------

levels(
  df_risco$PercepcaodeRisco
)

table(
  df_risco$PercepcaodeRisco,
  useNA = "ifany"
)


# ------------------------------------------------------------
# 6. CONTROLE DE OBSERVAÇÕES
# ------------------------------------------------------------

n_validos_risco <- sum(
  !is.na(
    df_risco$PercepcaodeRisco
  )
)

n_ausentes_risco <- sum(
  is.na(
    df_risco$PercepcaodeRisco
  )
)

n_validos_risco
n_ausentes_risco


# ------------------------------------------------------------
# 7. DISTRIBUIÇÃO DE FREQUÊNCIAS
# ------------------------------------------------------------

tabela_risco <- df_risco %>%
  filter(
    !is.na(PercepcaodeRisco)
  ) %>%
  count(
    PercepcaodeRisco,
    name = "Frequencia",
    .drop = FALSE
  ) %>%
  mutate(
    Porcentagem =
      Frequencia / sum(Frequencia) * 100
  )

print(tabela_risco)


# ------------------------------------------------------------
# 8. VISUALIZAÇÃO DA DISTRIBUIÇÃO
# ------------------------------------------------------------

grafico_risco <- ggplot(
  tabela_risco,
  aes(
    x = PercepcaodeRisco,
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
    title = "Percepção de Risco dos Respondentes",
    subtitle = paste0(
      "Respondentes válidos: n = ",
      n_validos_risco
    ),
    x = "Percepção de Risco",
    y = "Frequência Absoluta (n)",
    caption = "Fonte: Elaborado pela autora (2026)."
  ) +
  theme_minimal()

grafico_risco


# ------------------------------------------------------------
# FIM — V12 PERCEPÇÃO DE RISCO
# ------------------------------------------------------------
