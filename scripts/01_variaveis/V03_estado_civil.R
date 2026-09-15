# ============================================================
# V03 — ESTADO CIVIL DO(A) RESPONDENTE
# ============================================================
# Etapa: Refinamento do modelo / análise descritiva
#
# OBJETIVO:
# Caracterizar a distribuição do estado civil dos respondentes,
# identificando as categorias observadas, suas frequências e
# eventuais valores ausentes.
#
# IMPORTANTE:
# Esta etapa possui caráter descritivo e integra a
# caracterização da amostra.
# ============================================================


# ------------------------------------------------------------
# 1. SELEÇÃO DA VARIÁVEL
# ------------------------------------------------------------

df_estado_civil <- df %>%
  select(EstadoCivil)


# ------------------------------------------------------------
# 2. VALIDAÇÃO DAS CATEGORIAS
# ------------------------------------------------------------

# A inspeção inicial permite identificar as categorias efetivamente
# registradas e verificar a presença de valores ausentes.
#
# Nenhuma categoria deve ser alterada ou excluída antes dessa
# verificação.

unique(df_estado_civil$EstadoCivil)

table(
  df_estado_civil$EstadoCivil,
  useNA = "ifany"
)


# ------------------------------------------------------------
# 3. DISTRIBUIÇÃO DE FREQUÊNCIAS
# ------------------------------------------------------------

# A frequência absoluta representa o número de observações
# registrado em cada categoria.
#
# A frequência relativa permite comparar a participação das
# categorias entre as respostas válidas.

tabela_estado_civil <- df_estado_civil %>%
  filter(!is.na(EstadoCivil)) %>%
  count(EstadoCivil, name = "Frequencia") %>%
  mutate(
    Porcentagem = Frequencia / sum(Frequencia) * 100
  )

print(tabela_estado_civil)


# ------------------------------------------------------------
# 4. CONTROLE DE OBSERVAÇÕES VÁLIDAS E AUSENTES
# ------------------------------------------------------------

# A quantidade de valores válidos e ausentes é registrada
# separadamente para preservar a informação sobre perda amostral.

n_validos_estado_civil <- sum(
  !is.na(df_estado_civil$EstadoCivil)
)

n_ausentes_estado_civil <- sum(
  is.na(df_estado_civil$EstadoCivil)
)

n_validos_estado_civil
n_ausentes_estado_civil


# ------------------------------------------------------------
# 5. VISUALIZAÇÃO DA DISTRIBUIÇÃO
# ------------------------------------------------------------

# O gráfico apresenta as categorias observadas em ordem de
# frequência, preservando a distribuição original da variável.

grafico_estado_civil <- df_estado_civil %>%
  filter(!is.na(EstadoCivil)) %>%
  ggplot(
    aes(
      y = forcats::fct_infreq(EstadoCivil),
      fill = EstadoCivil
    )
  ) +
  geom_bar() +
  labs(
    title = "Distribuição por Estado Civil",
    x = "Frequência",
    y = "Estado Civil",
    caption = "Fonte: Elaborado pela autora (2026)."
  ) +
  theme_minimal() +
  theme(
    legend.position = "none"
  )

grafico_estado_civil


# ------------------------------------------------------------
# 6. AGRUPAMENTO PARA VISUALIZAÇÃO SIMPLIFICADA
# ------------------------------------------------------------

# Categorias com baixa frequência podem ser agrupadas para
# simplificar a representação gráfica.
#
# O agrupamento é utilizado apenas como recurso de visualização
# e não substitui a distribuição original apresentada anteriormente.

df_estado_civil <- df_estado_civil %>%
  mutate(
    EstadoCivil_Agrupado = forcats::fct_lump_n(
      EstadoCivil,
      n = 2,
      other_level = "Outros"
    )
  )


# ------------------------------------------------------------
# 7. VISUALIZAÇÃO SIMPLIFICADA
# ------------------------------------------------------------

grafico_estado_civil_agrupado <- df_estado_civil %>%
  filter(!is.na(EstadoCivil_Agrupado)) %>%
  ggplot(
    aes(
      x = forcats::fct_infreq(EstadoCivil_Agrupado),
      fill = EstadoCivil_Agrupado
    )
  ) +
  geom_bar() +
  geom_text(
    stat = "count",
    aes(label = after_stat(count)),
    vjust = -0.5
  ) +
  labs(
    title = "Distribuição Simplificada de Estado Civil",
    subtitle = "Categorias com menor frequência agrupadas em 'Outros'",
    x = "Estado Civil",
    y = "Quantidade (n)",
    caption = "Fonte: Elaborado pela autora (2026)."
  ) +
  theme_minimal() +
  theme(
    legend.position = "none"
  )

grafico_estado_civil_agrupado


# ------------------------------------------------------------
# FIM — V03 ESTADO CIVIL
# ------------------------------------------------------------
