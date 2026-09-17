# ============================================================
# V17 — EXPECTATIVA ATENDIDA
# ============================================================
# Etapa: Tratamento categórico / análise descritiva
#
# OBJETIVO:
# Caracterizar a experiência relatada pelos respondentes em
# relação ao acionamento do seguro e ao atendimento de suas
# expectativas.
#
# IMPORTANTE:
# A variável é categórica nominal. As diferentes respostas
# representam situações distintas de experiência com seguros
# e não possuem ordenação quantitativa.
# ============================================================


# ------------------------------------------------------------
# 1. SELEÇÃO DA VARIÁVEL
# ------------------------------------------------------------

df_expectativa <- df %>%
  transmute(
    ExpectativaAtendida =
      voce_ja_precisou_acionar_o_seguro_a_sua_expectativa_foi_atendida
  )


# ------------------------------------------------------------
# 2. INSPEÇÃO DAS RESPOSTAS ORIGINAIS
# ------------------------------------------------------------

head(
  df_expectativa$ExpectativaAtendida
)

unique(
  df_expectativa$ExpectativaAtendida
)

table(
  df_expectativa$ExpectativaAtendida,
  useNA = "ifany"
)


# ------------------------------------------------------------
# 3. PADRONIZAÇÃO TEXTUAL
# ------------------------------------------------------------

# As recodificações abaixo preservam o significado das respostas
# e padronizam apenas categorias identificadas no tratamento
# original.

df_expectativa <- df_expectativa %>%
  mutate(
    ExpectativaAtendida =
      stringr::str_squish(
        ExpectativaAtendida
      ),

    ExpectativaAtendida =
      case_when(

        ExpectativaAtendida == "não tenho" ~
          "Nunca contratei um seguro.",

        ExpectativaAtendida ==
          "Não acionei, pois não oferece cobertura." ~
          "Não acionei, sem cobertura",

        TRUE ~ ExpectativaAtendida
      )
  )


# ------------------------------------------------------------
# 4. CONFERÊNCIA APÓS A PADRONIZAÇÃO
# ------------------------------------------------------------

unique(
  df_expectativa$ExpectativaAtendida
)

table(
  df_expectativa$ExpectativaAtendida,
  useNA = "ifany"
)


# ------------------------------------------------------------
# 5. CONVERSÃO PARA VARIÁVEL CATEGÓRICA
# ------------------------------------------------------------

df_expectativa <- df_expectativa %>%
  mutate(
    ExpectativaAtendida =
      factor(
        ExpectativaAtendida
      )
  )


# ------------------------------------------------------------
# 6. CONTROLE DE OBSERVAÇÕES
# ------------------------------------------------------------

n_validos_expectativa <- sum(
  !is.na(
    df_expectativa$ExpectativaAtendida
  )
)

n_ausentes_expectativa <- sum(
  is.na(
    df_expectativa$ExpectativaAtendida
  )
)

n_validos_expectativa
n_ausentes_expectativa


# ------------------------------------------------------------
# 7. DISTRIBUIÇÃO DE FREQUÊNCIAS
# ------------------------------------------------------------

tabela_expectativa <- df_expectativa %>%
  filter(
    !is.na(ExpectativaAtendida)
  ) %>%
  count(
    ExpectativaAtendida,
    name = "Frequencia"
  ) %>%
  mutate(
    Porcentagem =
      Frequencia /
      sum(Frequencia) * 100
  ) %>%
  arrange(
    desc(Frequencia)
  )

print(tabela_expectativa)


# ------------------------------------------------------------
# 8. VISUALIZAÇÃO DA DISTRIBUIÇÃO
# ------------------------------------------------------------

grafico_expectativa <- ggplot(
  tabela_expectativa,
  aes(
    x = reorder(
      ExpectativaAtendida,
      Frequencia
    ),
    y = Frequencia
  )
) +
  geom_col(
    width = 0.7
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
    title = "Experiência e Expectativa com Seguros",
    subtitle = paste0(
      "Respondentes válidos: n = ",
      n_validos_expectativa
    ),
    x = "Categoria de Resposta",
    y = "Frequência Absoluta (n)",
    caption = "Fonte: Elaborado pela autora (2026)."
  ) +
  theme_minimal()

grafico_expectativa


# ------------------------------------------------------------
# FIM — V17 EXPECTATIVA ATENDIDA
# ------------------------------------------------------------
