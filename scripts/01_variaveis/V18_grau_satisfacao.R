# ============================================================
# V18 — GRAU DE SATISFAÇÃO
# ============================================================
# Etapa: Tratamento de variável ordinal / análise descritiva
#
# OBJETIVO:
# Caracterizar o grau de satisfação informado pelos
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

# No script original, Satisfação corresponde à penúltima
# coluna da base.
#
# Substituir futuramente esta seleção pelo nome exato da
# variável após identificação na base preparada.

df_satisfacao <- df %>%
  transmute(
    Satisfacao = .[[ncol(df) - 1]]
  )


# ------------------------------------------------------------
# 2. INSPEÇÃO DOS VALORES ORIGINAIS
# ------------------------------------------------------------

head(
  df_satisfacao$Satisfacao
)

unique(
  df_satisfacao$Satisfacao
)

table(
  df_satisfacao$Satisfacao,
  useNA = "ifany"
)


# ------------------------------------------------------------
# 3. CONVERSÃO PARA ESCALA NUMÉRICA
# ------------------------------------------------------------

df_satisfacao <- df_satisfacao %>%
  mutate(
    Satisfacao = as.numeric(
      as.character(Satisfacao)
    )
  )


# ------------------------------------------------------------
# 4. VALIDAÇÃO DA ESCALA
# ------------------------------------------------------------

# A escala esperada contém apenas valores inteiros de 1 a 5.

valores_invalidos_satisfacao <- df_satisfacao %>%
  filter(
    !is.na(Satisfacao),
    !Satisfacao %in% 1:5
  )

valores_invalidos_satisfacao


# ------------------------------------------------------------
# 5. CONTROLE DE OBSERVAÇÕES
# ------------------------------------------------------------

n_validos_satisfacao <- sum(
  df_satisfacao$Satisfacao %in% 1:5,
  na.rm = TRUE
)

n_ausentes_satisfacao <- sum(
  is.na(df_satisfacao$Satisfacao)
)

n_validos_satisfacao
n_ausentes_satisfacao


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

df_satisfacao <- df_satisfacao %>%
  mutate(
    Satisfacao_F = factor(
      Satisfacao,
      levels = 1:5,
      labels = labels_likert,
      ordered = TRUE
    )
  )


# ------------------------------------------------------------
# 7. DISTRIBUIÇÃO DE FREQUÊNCIAS
# ------------------------------------------------------------

tabela_satisfacao <- df_satisfacao %>%
  filter(
    !is.na(Satisfacao_F)
  ) %>%
  count(
    Satisfacao_F,
    name = "Frequencia",
    .drop = FALSE
  ) %>%
  mutate(
    Percentual =
      Frequencia / sum(Frequencia) * 100
  )

print(tabela_satisfacao)


# ------------------------------------------------------------
# 8. MEDIDAS DESCRITIVAS
# ------------------------------------------------------------

media_satisfacao <- mean(
  df_satisfacao$Satisfacao,
  na.rm = TRUE
)

mediana_satisfacao <- median(
  df_satisfacao$Satisfacao,
  na.rm = TRUE
)

desvio_padrao_satisfacao <- sd(
  df_satisfacao$Satisfacao,
  na.rm = TRUE
)

media_satisfacao
mediana_satisfacao
desvio_padrao_satisfacao


resumo_satisfacao <- tibble(
  N_Valido = n_validos_satisfacao,
  Media = media_satisfacao,
  Mediana = mediana_satisfacao,
  Desvio_Padrao = desvio_padrao_satisfacao
)

print(resumo_satisfacao)


# ------------------------------------------------------------
# 9. VISUALIZAÇÃO DA DISTRIBUIÇÃO
# ------------------------------------------------------------

grafico_satisfacao <- ggplot(
  tabela_satisfacao,
  aes(
    x = Satisfacao_F,
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
    title = "Distribuição do Grau de Satisfação",
    subtitle = paste0(
      "Respondentes válidos: n = ",
      n_validos_satisfacao
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

grafico_satisfacao


# ------------------------------------------------------------
# FIM — V18 GRAU DE SATISFAÇÃO
# ------------------------------------------------------------
