# ============================================================
# V13 — FAMÍLIA TEM OU JÁ TEVE SEGURO
# ============================================================
# Etapa: Tratamento categórico / análise descritiva
#
# OBJETIVO:
# Caracterizar o histórico de consumo de seguros das famílias
# dos respondentes, identificando se alguém da família tem ou
# já teve seguro.
#
# IMPORTANTE:
# Esta é uma variável categórica nominal e constitui uma das
# principais medidas de consumo de seguros utilizadas na análise.
#
# As categorias observadas são:
# "Sim", "Não" e "Não sei informar".
# ============================================================


# ------------------------------------------------------------
# 1. SELEÇÃO DA VARIÁVEL
# ------------------------------------------------------------

df_seguro <- df %>%
  transmute(
    TemSeguro_TeveSeguro =
      alguem_da_familia_pais_e_irmaos_do_a_entrevistado_a_tem_ou_ja_teve_seguro
  )


# ------------------------------------------------------------
# 2. INSPEÇÃO DAS RESPOSTAS ORIGINAIS
# ------------------------------------------------------------

unique(
  df_seguro$TemSeguro_TeveSeguro
)

table(
  df_seguro$TemSeguro_TeveSeguro,
  useNA = "ifany"
)


# ------------------------------------------------------------
# 3. PADRONIZAÇÃO DAS CATEGORIAS
# ------------------------------------------------------------

# A ordem abaixo é utilizada apenas para apresentação.
# A variável não é tratada como ordinal.

categorias_seguro <- c(
  "Sim",
  "Não",
  "Não sei informar"
)

df_seguro <- df_seguro %>%
  mutate(
    TemSeguro_TeveSeguro = factor(
      TemSeguro_TeveSeguro,
      levels = categorias_seguro,
      ordered = FALSE
    )
  )


# ------------------------------------------------------------
# 4. CONFERÊNCIA APÓS A CONVERSÃO
# ------------------------------------------------------------

levels(
  df_seguro$TemSeguro_TeveSeguro
)

table(
  df_seguro$TemSeguro_TeveSeguro,
  useNA = "ifany"
)


# ------------------------------------------------------------
# 5. CONTROLE DE OBSERVAÇÕES
# ------------------------------------------------------------

n_validos_seguro <- sum(
  !is.na(
    df_seguro$TemSeguro_TeveSeguro
  )
)

n_ausentes_seguro <- sum(
  is.na(
    df_seguro$TemSeguro_TeveSeguro
  )
)

n_validos_seguro
n_ausentes_seguro


# ------------------------------------------------------------
# 6. DISTRIBUIÇÃO DE FREQUÊNCIAS
# ------------------------------------------------------------

tabela_seguro <- df_seguro %>%
  filter(
    !is.na(TemSeguro_TeveSeguro)
  ) %>%
  count(
    TemSeguro_TeveSeguro,
    name = "Frequencia",
    .drop = FALSE
  ) %>%
  mutate(
    Porcentagem =
      Frequencia / sum(Frequencia) * 100
  )

print(tabela_seguro)


# ------------------------------------------------------------
# 7. VISUALIZAÇÃO DA DISTRIBUIÇÃO
# ------------------------------------------------------------

grafico_seguro <- ggplot(
  tabela_seguro,
  aes(
    x = TemSeguro_TeveSeguro,
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
    title = "Histórico de Seguro na Família",
    subtitle = paste0(
      "Respondentes válidos: n = ",
      n_validos_seguro
    ),
    x = "Família tem ou já teve seguro?",
    y = "Frequência Absoluta (n)",
    caption = "Fonte: Elaborado pela autora (2026)."
  ) +
  theme_minimal()

grafico_seguro


# ------------------------------------------------------------
# FIM — V13 FAMÍLIA TEM OU JÁ TEVE SEGURO
# ------------------------------------------------------------
