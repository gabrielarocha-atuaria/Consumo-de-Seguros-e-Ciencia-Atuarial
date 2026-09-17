# ============================================================
# V15 — FATORES PARA CONTRATAÇÃO DE SEGURO
# ============================================================
# Etapa: Tratamento de respostas múltiplas / análise descritiva
#
# OBJETIVO:
# Identificar e descrever os fatores considerados importantes
# pelas famílias dos respondentes para a contratação de seguros.
#
# IMPORTANTE:
# A pergunta permite múltiplas respostas. Portanto, cada fator
# selecionado é tratado como uma variável indicadora:
#
# 1 = fator selecionado
# 0 = fator não selecionado
#
# Como um mesmo respondente pode selecionar mais de um fator,
# a soma dos percentuais pode ultrapassar 100%.
# ============================================================


# ------------------------------------------------------------
# 1. SELEÇÃO DA VARIÁVEL
# ------------------------------------------------------------

df_fatores <- df %>%
  transmute(
    id_respondente = row_number(),
    FatoresparaContratar =
      assinale_os_fatores_considerados_importantes_pela_sua_familia_para_contratar_um_seguro
  )


# ------------------------------------------------------------
# 2. INSPEÇÃO DAS RESPOSTAS ORIGINAIS
# ------------------------------------------------------------

head(
  df_fatores$FatoresparaContratar
)

unique(
  df_fatores$FatoresparaContratar
)

table(
  is.na(df_fatores$FatoresparaContratar)
)


# ------------------------------------------------------------
# 3. LIMPEZA DAS RESPOSTAS
# ------------------------------------------------------------

df_fatores <- df_fatores %>%
  mutate(
    Fatores_Limpo = stringr::str_replace(
      FatoresparaContratar,
      ",\\s*$",
      ""
    ),
    Fatores_Limpo = stringr::str_trim(
      Fatores_Limpo
    )
  )


# ------------------------------------------------------------
# 4. CONTROLE DE OBSERVAÇÕES
# ------------------------------------------------------------

n_validos_fatores <- sum(
  !is.na(df_fatores$Fatores_Limpo) &
    df_fatores$Fatores_Limpo != ""
)

n_ausentes_fatores <- sum(
  is.na(df_fatores$Fatores_Limpo) |
    df_fatores$Fatores_Limpo == ""
)

n_validos_fatores
n_ausentes_fatores


# ------------------------------------------------------------
# 5. SEPARAÇÃO DAS RESPOSTAS MÚLTIPLAS
# ------------------------------------------------------------

# Cada fator selecionado passa a ocupar uma linha.
# O identificador permite manter a referência ao respondente
# original após a separação.

df_fatores_long <- df_fatores %>%
  filter(
    !is.na(Fatores_Limpo),
    Fatores_Limpo != ""
  ) %>%
  tidyr::separate_rows(
    Fatores_Limpo,
    sep = ",\\s*"
  ) %>%
  mutate(
    Fatores_Limpo =
      stringr::str_trim(Fatores_Limpo)
  )


# ------------------------------------------------------------
# 6. CONFERÊNCIA DOS FATORES IDENTIFICADOS
# ------------------------------------------------------------

sort(
  unique(df_fatores_long$Fatores_Limpo)
)

table(
  df_fatores_long$Fatores_Limpo
)


# ------------------------------------------------------------
# 7. CRIAÇÃO DAS VARIÁVEIS DUMMY
# ------------------------------------------------------------

# Cada fator é transformado em indicador binário:
# 1 = selecionado pelo respondente
# 0 = não selecionado.

df_fatores_dummy <- df_fatores_long %>%
  distinct(
    id_respondente,
    Fatores_Limpo
  ) %>%
  mutate(
    presente = 1L
  ) %>%
  pivot_wider(
    id_cols = id_respondente,
    names_from = Fatores_Limpo,
    values_from = presente,
    values_fill = 0,
    names_prefix = "Fator_"
  )


# ------------------------------------------------------------
# 8. CONFERÊNCIA DAS DUMMIES CRIADAS
# ------------------------------------------------------------

names(
  df_fatores_dummy %>%
    select(starts_with("Fator_"))
)


# ------------------------------------------------------------
# 9. FREQUÊNCIA DOS FATORES
# ------------------------------------------------------------

# Frequência = número de respondentes que selecionaram o fator.
#
# Percentual = proporção dos respondentes válidos que
# selecionaram cada fator.
#
# Os percentuais não precisam somar 100%, pois a pergunta
# permite múltiplas escolhas.

resumo_fatores <- df_fatores_dummy %>%
  select(
    starts_with("Fator_")
  ) %>%
  summarise(
    across(
      everything(),
      sum
    )
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Fator",
    values_to = "Frequencia"
  ) %>%
  mutate(
    Fator = stringr::str_remove(
      Fator,
      "^Fator_"
    ),
    Percentual =
      Frequencia / n_validos_fatores * 100
  ) %>%
  arrange(
    desc(Frequencia)
  )

print(resumo_fatores)


# ------------------------------------------------------------
# 10. VISUALIZAÇÃO DOS FATORES
# ------------------------------------------------------------

grafico_fatores <- ggplot(
  resumo_fatores,
  aes(
    x = reorder(
      Fator,
      Percentual
    ),
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
    title = "Fatores Considerados na Contratação de Seguros",
    subtitle = paste0(
      "Respondentes válidos: n = ",
      n_validos_fatores,
      " | Resposta múltipla"
    ),
    x = "Fator",
    y = "Percentual de Respondentes (%)",
    caption = "Fonte: Elaborado pela autora (2026)."
  ) +
  theme_minimal()

grafico_fatores


# ------------------------------------------------------------
# FIM — V15 FATORES PARA CONTRATAÇÃO
# ------------------------------------------------------------
