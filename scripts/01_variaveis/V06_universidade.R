# ============================================================
# V06 — UNIVERSIDADE DO(A) RESPONDENTE
# ============================================================
# Etapa: Tratamento textual / análise descritiva
#
# OBJETIVO:
# Padronizar as informações referentes à instituição de ensino
# dos respondentes e descrever sua distribuição na amostra.
#
# IMPORTANTE:
# A padronização busca reunir diferentes formas de escrita
# referentes à mesma instituição, preservando as respostas
# originais para conferência.
# ============================================================


# ------------------------------------------------------------
# 1. SELEÇÃO DA VARIÁVEL
# ------------------------------------------------------------

df_universidade <- df %>%
  select(universidade)


# ------------------------------------------------------------
# 2. INSPEÇÃO DAS RESPOSTAS ORIGINAIS
# ------------------------------------------------------------

# A inspeção inicial permite identificar diferentes formas de
# escrita, abreviações e respostas que não correspondem a
# instituições de ensino.

unique(df_universidade$universidade)

table(
  df_universidade$universidade,
  useNA = "ifany"
)


# ------------------------------------------------------------
# 3. NORMALIZAÇÃO TEXTUAL
# ------------------------------------------------------------

# É criada uma variável auxiliar para facilitar a comparação
# entre respostas textualmente diferentes.
#
# A normalização:
# - remove espaços excedentes;
# - converte o texto para caixa alta;
# - remove diferenças de acentuação.
#
# A resposta original permanece preservada em "universidade".

df_universidade <- df_universidade %>%
  mutate(
    universidade_clean = universidade %>%
      stringr::str_trim() %>%
      stringr::str_to_upper() %>%
      iconv(
        from = "UTF-8",
        to = "ASCII//TRANSLIT"
      )
  )


# ------------------------------------------------------------
# 4. IDENTIFICAÇÃO DE RESPOSTAS INVÁLIDAS
# ------------------------------------------------------------

# Respostas que não representam instituições de ensino são
# classificadas como ausentes para a análise da variável.
#
# A transformação é realizada somente na variável tratada,
# mantendo a resposta original disponível para conferência.

df_universidade <- df_universidade %>%
  mutate(
    universidade_clean = case_when(

      universidade_clean %in% c(
        "SIM",
        "NAO"
      ) ~ NA_character_,

      TRUE ~ universidade_clean
    )
  )


# ------------------------------------------------------------
# 5. PADRONIZAÇÃO DAS INSTITUIÇÕES
# ------------------------------------------------------------

# Diferentes formas de registrar uma mesma instituição são
# reunidas sob uma denominação comum.
#
# As demais respostas válidas são preservadas após a
# normalização textual.

df_universidade <- df_universidade %>%
  mutate(
    universidade_final = case_when(

      str_detect(
        universidade_clean,
        "UFPB|FEDERAL DA PARAIBA|FEDERAL DA PARAIBA CAMPUS I"
      ) ~ "UFPB",

      str_detect(
        universidade_clean,
        "UFPE"
      ) ~ "UFPE",

      str_detect(
        universidade_clean,
        "CRUZEIRO"
      ) ~ "UNIV. CRUZEIRO DO SUL",

      TRUE ~ universidade_clean
    )
  )


# ------------------------------------------------------------
# 6. CONFERÊNCIA DA PADRONIZAÇÃO
# ------------------------------------------------------------

# A tabela relaciona a resposta original à categoria final.
# Isso permite verificar as regras de transformação antes
# da produção das frequências consolidadas.

tabela_conferencia_universidade <- df_universidade %>%
  count(
    universidade,
    universidade_final,
    sort = TRUE
  )

print(
  tabela_conferencia_universidade,
  n = Inf
)


# ------------------------------------------------------------
# 7. CONTROLE DE RESPOSTAS VÁLIDAS E AUSENTES
# ------------------------------------------------------------

n_validos_universidade <- sum(
  !is.na(df_universidade$universidade_final)
)

n_ausentes_universidade <- sum(
  is.na(df_universidade$universidade_final)
)

n_validos_universidade
n_ausentes_universidade


# ------------------------------------------------------------
# 8. DISTRIBUIÇÃO DE FREQUÊNCIAS
# ------------------------------------------------------------

# A distribuição considera apenas as respostas que puderam ser
# classificadas como instituições de ensino válidas.

tabela_universidade <- df_universidade %>%
  filter(
    !is.na(universidade_final)
  ) %>%
  count(
    universidade_final,
    name = "Frequencia"
  ) %>%
  mutate(
    Porcentagem =
      Frequencia / sum(Frequencia) * 100
  ) %>%
  arrange(
    desc(Frequencia)
  )

print(tabela_universidade)


# ------------------------------------------------------------
# 9. VISUALIZAÇÃO DA DISTRIBUIÇÃO
# ------------------------------------------------------------

grafico_universidade <- ggplot(
  tabela_universidade,
  aes(
    x = reorder(
      universidade_final,
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
        round(Porcentagem, 1),
        "%"
      )
    ),
    hjust = -0.2,
    size = 4,
    fontface = "bold"
  ) +
  coord_flip() +
  scale_y_continuous(
    limits = c(
      0,
      max(tabela_universidade$Frequencia) * 1.1
    )
  ) +
  labs(
    title = "Distribuição da Amostra por Instituição de Ensino",
    subtitle = paste0(
      "Total de respondentes válidos: n = ",
      sum(tabela_universidade$Frequencia)
    ),
    x = "Universidade",
    y = "Frequência Absoluta (n)",
    caption = "Fonte: Elaborado pela autora (2026)."
  ) +
  theme_minimal()

grafico_universidade


# ------------------------------------------------------------
# FIM — V06 UNIVERSIDADE
# ------------------------------------------------------------
