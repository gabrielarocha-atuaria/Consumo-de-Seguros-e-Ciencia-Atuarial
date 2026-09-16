# ============================================================
# V09 — NÚMERO DE MEMBROS DA FAMÍLIA
# ============================================================
# Etapa: Tratamento numérico / análise descritiva
#
# OBJETIVO:
# Caracterizar o número de membros das famílias dos respondentes
# por meio de medidas de posição, dispersão e distribuição de
# frequências.
#
# IMPORTANTE:
# O número de membros é tratado como variável quantitativa
# discreta. A resposta original é preservada antes da conversão
# para formato numérico.
# ============================================================


# ------------------------------------------------------------
# 1. SELEÇÃO DA VARIÁVEL
# ------------------------------------------------------------

# ATENÇÃO:
# Substituir "NOME_EXATO_DA_COLUNA" pelo nome da variável
# correspondente após a aplicação de clean_names() em
# preparacao_base.R.
#
# Não utilizar a posição da coluna (ex.: coluna 11), pois a
# posição pode mudar caso a estrutura da base seja alterada.

df_membros <- df %>%
  transmute(
    n_membros_original = NOME_EXATO_DA_COLUNA
  )


# ------------------------------------------------------------
# 2. INSPEÇÃO DAS RESPOSTAS ORIGINAIS
# ------------------------------------------------------------

unique(
  df_membros$n_membros_original
)

table(
  df_membros$n_membros_original,
  useNA = "ifany"
)


# ------------------------------------------------------------
# 3. EXTRAÇÃO DO VALOR NUMÉRICO
# ------------------------------------------------------------

# Extrai a primeira sequência numérica presente na resposta.
#
# Esse procedimento permite tratar respostas que contenham
# simultaneamente número e texto, preservando a resposta
# original para conferência.

df_membros <- df_membros %>%
  mutate(
    n_membros = stringr::str_extract(
      as.character(n_membros_original),
      "\\d+"
    ),
    n_membros = as.numeric(n_membros)
  )


# ------------------------------------------------------------
# 4. CONFERÊNCIA DA CONVERSÃO
# ------------------------------------------------------------

tabela_conferencia_membros <- df_membros %>%
  count(
    n_membros_original,
    n_membros,
    sort = TRUE
  )

print(
  tabela_conferencia_membros,
  n = Inf
)


# ------------------------------------------------------------
# 5. CONTROLE DE OBSERVAÇÕES
# ------------------------------------------------------------

n_validos_membros <- sum(
  !is.na(df_membros$n_membros)
)

n_ausentes_membros <- sum(
  is.na(df_membros$n_membros)
)

n_validos_membros
n_ausentes_membros


# ------------------------------------------------------------
# 6. BASE ANALÍTICA
# ------------------------------------------------------------

# Para os cálculos quantitativos são consideradas apenas
# observações com número de membros identificado.

df_membros_analise <- df_membros %>%
  filter(
    !is.na(n_membros)
  )


# ------------------------------------------------------------
# 7. RESUMO ESTATÍSTICO
# ------------------------------------------------------------

summary(
  df_membros_analise$n_membros
)


# ------------------------------------------------------------
# 8. ESTATÍSTICAS DESCRITIVAS
# ------------------------------------------------------------

media_membros <- mean(
  df_membros_analise$n_membros
)

mediana_membros <- median(
  df_membros_analise$n_membros
)

desvio_membros <- sd(
  df_membros_analise$n_membros
)

variancia_membros <- var(
  df_membros_analise$n_membros
)

minimo_membros <- min(
  df_membros_analise$n_membros
)

maximo_membros <- max(
  df_membros_analise$n_membros
)

cv_membros <- (
  desvio_membros / media_membros
) * 100


# ------------------------------------------------------------
# 9. TABELA-RESUMO DAS ESTATÍSTICAS
# ------------------------------------------------------------

estatisticas_membros <- tibble(
  N_validos = n_validos_membros,
  Media = media_membros,
  Mediana = mediana_membros,
  Desvio_Padrao = desvio_membros,
  Variancia = variancia_membros,
  Minimo = minimo_membros,
  Maximo = maximo_membros,
  CV_percentual = cv_membros
)

print(estatisticas_membros)


# ------------------------------------------------------------
# 10. DISTRIBUIÇÃO DE FREQUÊNCIAS
# ------------------------------------------------------------

tabela_membros <- df_membros_analise %>%
  count(
    n_membros,
    name = "Frequencia"
  ) %>%
  mutate(
    Porcentagem =
      Frequencia / sum(Frequencia) * 100
  ) %>%
  arrange(n_membros)

print(tabela_membros)


# ------------------------------------------------------------
# 11. BOXPLOT
# ------------------------------------------------------------

grafico_boxplot_membros <- ggplot(
  df_membros_analise,
  aes(
    y = n_membros
  )
) +
  geom_boxplot() +
  labs(
    title = "Dispersão do Número de Membros da Família",
    x = NULL,
    y = "Quantidade de Membros",
    caption = "Fonte: Elaborado pela autora (2026)."
  ) +
  theme_minimal()

grafico_boxplot_membros


# ------------------------------------------------------------
# 12. DISTRIBUIÇÃO DO NÚMERO DE MEMBROS
# ------------------------------------------------------------

grafico_membros <- ggplot(
  df_membros_analise,
  aes(
    x = factor(n_membros)
  )
) +
  geom_bar() +
  geom_text(
    stat = "count",
    aes(
      label = after_stat(count)
    ),
    vjust = -0.5
  ) +
  scale_y_continuous(
    expand = expansion(
      mult = c(0, 0.10)
    )
  ) +
  labs(
    title = "Distribuição do Número de Membros da Família",
    subtitle = paste0(
      "Respondentes válidos: n = ",
      n_validos_membros
    ),
    x = "Quantidade de Membros",
    y = "Número de Famílias (Frequência)",
    caption = "Fonte: Elaborado pela autora (2026)."
  ) +
  theme_minimal()

grafico_membros


# ------------------------------------------------------------
# FIM — V09 NÚMERO DE MEMBROS DA FAMÍLIA
# ------------------------------------------------------------
