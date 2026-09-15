# ============================================================
# V05 — RELIGIÃO DO(A) RESPONDENTE
# ============================================================
# Etapa: Tratamento categórico / análise descritiva
#
# OBJETIVO:
# Caracterizar a distribuição das respostas referentes à religião
# e avaliar formas de consolidação das categorias observadas.
#
# IMPORTANTE:
# A variável foi coletada de forma categórica, mas apresentou
# respostas com diferentes denominações. Os agrupamentos realizados
# são explicitados no código para preservar a rastreabilidade
# das decisões de tratamento.
# ============================================================


# ------------------------------------------------------------
# 1. SELEÇÃO DA VARIÁVEL
# ------------------------------------------------------------

df_religiao <- df %>%
  select(religiao)


# ------------------------------------------------------------
# 2. INSPEÇÃO DAS CATEGORIAS ORIGINAIS
# ------------------------------------------------------------

# Antes de qualquer agrupamento, são preservadas e examinadas
# as categorias registradas na base.

unique(df_religiao$religiao)

table(
  df_religiao$religiao,
  useNA = "ifany"
)


# ------------------------------------------------------------
# 3. DISTRIBUIÇÃO ORIGINAL DE FREQUÊNCIAS
# ------------------------------------------------------------

# A tabela apresenta a frequência absoluta e relativa das
# categorias antes da aplicação de qualquer agrupamento.

tabela_frequencia_religiao <- df_religiao %>%
  count(religiao) %>%
  mutate(
    percentual = n / sum(n) * 100,
    label = paste0(
      round(percentual, 1),
      "%"
    )
  ) %>%
  arrange(desc(n))

print(tabela_frequencia_religiao)


# ------------------------------------------------------------
# 4. AGRUPAMENTO POR FREQUÊNCIA
# ------------------------------------------------------------

# Para reduzir a fragmentação de categorias pouco frequentes,
# são mantidas as quatro categorias com maior frequência.
#
# As demais são reunidas em "Outros".
#
# Esse procedimento considera exclusivamente a frequência
# observada e não pressupõe equivalência conceitual entre
# as categorias agrupadas.

df_religiao_agrupada <- df_religiao %>%
  mutate(
    religiao_agrupada = forcats::fct_lump_n(
      religiao,
      n = 4,
      other_level = "Outros"
    )
  )


# ------------------------------------------------------------
# 5. DISTRIBUIÇÃO APÓS AGRUPAMENTO
# ------------------------------------------------------------

tabela_religiao_agrupada <- df_religiao_agrupada %>%
  count(religiao_agrupada) %>%
  mutate(
    percentual = n / sum(n) * 100,
    label = paste0(
      round(percentual, 1),
      "%"
    )
  ) %>%
  arrange(desc(n))

print(tabela_religiao_agrupada)


# ------------------------------------------------------------
# 6. RECODIFICAÇÃO EXPLORATÓRIA DAS CATEGORIAS
# ------------------------------------------------------------

# Durante o refinamento também foi examinada uma consolidação
# baseada no conteúdo declarado nas respostas.
#
# Diferentemente do agrupamento anterior, esta etapa estabelece
# regras explícitas de recodificação e, por isso, é mantida
# separada da classificação baseada apenas em frequência.

df_religiao_recodificada <- df_religiao %>%
  mutate(
    religiao_rec = case_when(

      religiao %in% c(
        "Evangélica",
        "Crente",
        "Crista"
      ) ~ "Evangélica",

      religiao %in% c(
        "Agnóstica",
        "Ateia",
        "Não possuo.",
        "Sem religião"
      ) ~ "Sem Religião/Ateu/Agnóstico",

      religiao == "Católica" ~ "Católica",

      TRUE ~ "Outras"
    )
  )


# ------------------------------------------------------------
# 7. CONFERÊNCIA DA RECODIFICAÇÃO
# ------------------------------------------------------------

tabela_religiao_recodificada <- df_religiao_recodificada %>%
  count(religiao_rec) %>%
  mutate(
    percentual = n / sum(n) * 100,
    label = paste0(
      round(percentual, 1),
      "%"
    )
  ) %>%
  arrange(desc(n))

print(tabela_religiao_recodificada)


# Conferência das respostas originais classificadas como "Outras".
#
# Esta etapa permite verificar quais categorias foram reunidas
# pela regra de recodificação.

conferencia_outras_religioes <- df_religiao_recodificada %>%
  filter(religiao_rec == "Outras") %>%
  count(religiao, sort = TRUE)

print(conferencia_outras_religioes)


# ------------------------------------------------------------
# 8. VISUALIZAÇÃO DA DISTRIBUIÇÃO AGRUPADA
# ------------------------------------------------------------

# O gráfico utiliza o agrupamento baseado em frequência,
# preservando as quatro categorias mais frequentes e reunindo
# as demais em "Outros".

grafico_religiao <- ggplot(
  tabela_religiao_agrupada,
  aes(
    x = reorder(
      religiao_agrupada,
      -n
    ),
    y = n,
    fill = religiao_agrupada
  )
) +
  geom_col(
    width = 0.7,
    color = "black"
  ) +
  geom_text(
    aes(label = label),
    vjust = -0.5,
    fontface = "bold",
    size = 5
  ) +
  scale_y_continuous(
    expand = expansion(
      mult = c(0, 0.2)
    )
  ) +
  labs(
    title = "Distribuição da Crença Religiosa",
    subtitle = paste0(
      "Amostra total (n = ",
      sum(tabela_religiao_agrupada$n),
      ")"
    ),
    x = "Religião",
    y = "Frequência Absoluta",
    caption = "Fonte: Elaborado pela autora (2026)."
  ) +
  theme_classic(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title = element_text(
      face = "bold"
    )
  )

grafico_religiao


# ------------------------------------------------------------
# FIM — V05 RELIGIÃO
# ------------------------------------------------------------
