# ============================================================
# V19 — RECOMENDAÇÃO
# ============================================================
# Etapa: Tratamento de variável ordinal / análise descritiva
#
# OBJETIVO:
# Caracterizar o nível de recomendação informado pelos
# respondentes em relação à experiência com seguros.
#
# IMPORTANTE:
# A variável utiliza escala Likert de 1 a 5:
#
# 1 = Muito Baixo
# 2 = Baixo
# 3 = Neutro
# 4 = Alto
# 5 = Muito Alto
#
# A escala possui ordenação entre as categorias, embora as
# distâncias entre os níveis não devam ser interpretadas
# automaticamente como intervalos quantitativos equivalentes.
# ============================================================


# ------------------------------------------------------------
# 1. SELEÇÃO DA VARIÁVEL
# ------------------------------------------------------------

# No script original, Recomendação corresponde à última
# coluna da base.
#
# Substituir futuramente esta seleção pelo nome exato da
# variável após identificação na base preparada.

df_recomendacao <- df %>%
  transmute(
    Recomendacao = .[[ncol(df)]]
  )


# ------------------------------------------------------------
# 2. INSPEÇÃO DOS VALORES ORIGINAIS
# ------------------------------------------------------------

head(
  df_recomendacao$Recomendacao
)

unique(
  df_recomendacao$Recomendacao
)

table(
  df_recomendacao$Recomendacao,
  useNA = "ifany"
)


# ------------------------------------------------------------
# 3. CONVERSÃO PARA ESCALA NUMÉRICA
# ------------------------------------------------------------

df_recomendacao <- df_recomendacao %>%
  mutate(
    Recomendacao = as.numeric(
      as.character(Recomendacao)
    )
  )


# ------------------------------------------------------------
# 4. VALIDAÇÃO DA ESCALA
# ------------------------------------------------------------

# A escala esperada contém apenas valores inteiros de 1 a 5.

valores_invalidos_recomendacao <- df_recomendacao %>%
  filter(
    !is.na(Recomendacao),
    !Recomendacao %in% 1:5
  )

valores_invalidos_recomendacao


# ------------------------------------------------------------
# 5. CONTROLE DE OBSERVAÇÕES
# ------------------------------------------------------------

n_validos_recomendacao <- sum(
  df_recomendacao$Recomendacao %in% 1:5,
  na.rm = TRUE
)

n_ausentes_recomendacao <- sum(
  is.na(df_recomendacao$Recomendacao)
)

n_validos_recomendacao
n_ausentes_recomendacao


# ------------------------------------------------------------
# 6. CRIAÇÃO DA VERSÃO ORDINAL
# ------------------------------------------------------------

labels_likert <- c(
  "1. Muito Baixo",
  "2. Baixo",
  "3. Neutro",
  "4. Alto",
  "5. Muito Alto"
)

df_recomendacao <- df_recomendacao %>%
  mutate(
    Recomendacao_F = factor(
      Recomendacao,
      levels = 1:5,
      labels = labels_likert,
      ordered = TRUE
    )
  )


# ------------------------------------------------------------
# 7. DISTRIBUIÇÃO DE FREQUÊNCIAS
# ------------------------------------------------------------

tabela_recomendacao <- df_recomendacao %>%
  filter(
    !is.na(Recomendacao_F)
  ) %>%
  count(
    Recomendacao_F,
    name = "Frequencia",
    .drop = FALSE
  ) %>%
  mutate(
    Percentual =
      Frequencia / sum(Frequencia) * 100
  )

print(tabela_recomendacao)


# ------------------------------------------------------------
# 8. MEDIDAS DESCRITIVAS
# ------------------------------------------------------------

media_recomendacao <- mean(
  df_recomendacao$Recomendacao,
  na.rm = TRUE
)

mediana_recomendacao <- median(
  df_recomendacao$Recomendacao,
  na.rm = TRUE
)

desvio_padrao_recomendacao <- sd(
  df_recomendacao$Recomendacao,
  na.rm = TRUE
)

media_recomendacao
mediana_recomendacao
desvio_padrao_recomendacao


resumo_recomendacao <- tibble(
  N_Valido = n_validos_recomendacao,
  Media = media_recomendacao,
  Mediana = mediana_recomendacao,
  Desvio_Padrao = desvio_padrao_recomendacao
)

print(resumo_recomendacao)


# ------------------------------------------------------------
# 9. VISUALIZAÇÃO DA DISTRIBUIÇÃO
# ------------------------------------------------------------

grafico_recomendacao <- ggplot(
  tabela_recomendacao,
  aes(
    x = Recomendacao_F,
    y = Percentual
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
        round(Percentual, 1),
        "%)"
      )
    ),
    vjust = -0.5,
    size = 3.5
  ) +
  scale_y_continuous(
    expand = expansion(
      mult = c(0, 0.15)
    )
  ) +
  labs(
    title = "Distribuição do Nível de Recomendação",
    subtitle = paste0(
      "Respondentes válidos: n = ",
      n_validos_recomendacao
    ),
    x = "Escala Likert",
    y = "Percentual de Respondentes (%)",
    caption = "Fonte: Elaborado pela autora (2026)."
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(
      angle = 25,
      hjust = 1
    )
  )

grafico_recomendacao


# ------------------------------------------------------------
# FIM — V19 RECOMENDAÇÃO
# ------------------------------------------------------------
