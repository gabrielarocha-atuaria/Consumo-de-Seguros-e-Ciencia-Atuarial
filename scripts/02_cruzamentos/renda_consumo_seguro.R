# ============================================================
# RENDA × CONSUMO DE SEGUROS
# ============================================================
# Etapa: Análise bivariada / associação
#
# OBJETIVO:
# Analisar a distribuição do consumo de seguros segundo
# as faixas de renda per capita familiar observadas na amostra.
#
# A associação entre as variáveis é avaliada por meio de
# tabela de contingência e teste de independência.
#
# IMPORTANTE:
# A identificação de associação estatística não estabelece
# relação causal entre renda e consumo de seguros.
# ============================================================


# ------------------------------------------------------------
# 1. SELEÇÃO DAS VARIÁVEIS
# ------------------------------------------------------------

df_renda_seguro <- df %>%
  transmute(
    renda =
      qual_e_a_renda_per_capita_da_familia,

    TemSeguro_TeveSeguro =
      alguem_da_familia_pais_e_irmaos_do_a_entrevistado_a_tem_ou_ja_teve_seguro
  )


# ------------------------------------------------------------
# 2. INSPEÇÃO DAS VARIÁVEIS
# ------------------------------------------------------------

unique(
  df_renda_seguro$renda
)

unique(
  df_renda_seguro$TemSeguro_TeveSeguro
)

table(
  df_renda_seguro$renda,
  useNA = "ifany"
)

table(
  df_renda_seguro$TemSeguro_TeveSeguro,
  useNA = "ifany"
)


# ------------------------------------------------------------
# 3. PADRONIZAÇÃO DA RENDA
# ------------------------------------------------------------

ordem_renda <- c(
  "Até 1 Salário Mínimo (SM)",
  "Entre 1 e 3 SM",
  "Entre 3 e 5 SM",
  "Entre 5 e 7 SM",
  "Acima de 7 SM"
)

df_renda_seguro <- df_renda_seguro %>%
  mutate(
    renda = factor(
      renda,
      levels = ordem_renda,
      ordered = TRUE
    )
  )


# ------------------------------------------------------------
# 4. PADRONIZAÇÃO DO CONSUMO DE SEGUROS
# ------------------------------------------------------------

# A variável é nominal.
# "Não sei informar" é uma resposta válida e é preservada
# na análise da distribuição observada.

df_renda_seguro <- df_renda_seguro %>%
  mutate(
    TemSeguro_TeveSeguro = factor(
      TemSeguro_TeveSeguro,
      levels = c(
        "Não",
        "Não sei informar",
        "Sim"
      )
    )
  )


# ------------------------------------------------------------
# 5. CONTROLE DAS OBSERVAÇÕES VÁLIDAS
# ------------------------------------------------------------

df_renda_seguro_analise <- df_renda_seguro %>%
  filter(
    !is.na(renda),
    !is.na(TemSeguro_TeveSeguro)
  )

n_validos <- nrow(
  df_renda_seguro_analise
)

n_excluidos <- nrow(df_renda_seguro) - n_validos

n_validos
n_excluidos


# ------------------------------------------------------------
# 6. TABELA DE CONTINGÊNCIA
# ------------------------------------------------------------

tabela_renda_seguro <- table(
  df_renda_seguro_analise$renda,
  df_renda_seguro_analise$TemSeguro_TeveSeguro
)

print(
  tabela_renda_seguro
)


# ------------------------------------------------------------
# 7. DISTRIBUIÇÃO PERCENTUAL POR FAIXA DE RENDA
# ------------------------------------------------------------

# Os percentuais são calculados dentro de cada faixa de renda.
# Portanto, cada linha totaliza aproximadamente 100%.

prop_renda_seguro <- prop.table(
  tabela_renda_seguro,
  margin = 1
) * 100

print(
  round(
    prop_renda_seguro,
    2
  )
)


# ------------------------------------------------------------
# 8. TABELA PARA APRESENTAÇÃO
# ------------------------------------------------------------

tabela_renda_seguro_df <- as.data.frame(
  tabela_renda_seguro
) %>%
  rename(
    Renda = Var1,
    Consumo = Var2,
    Frequencia = Freq
  ) %>%
  group_by(
    Renda
  ) %>%
  mutate(
    Percentual =
      Frequencia /
      sum(Frequencia) * 100
  ) %>%
  ungroup()

print(
  tabela_renda_seguro_df
)


# ------------------------------------------------------------
# 9. TESTE DE INDEPENDÊNCIA
# ------------------------------------------------------------

# H0:
# Renda e consumo de seguros são independentes na amostra.
#
# H1:
# Existe associação entre renda e consumo de seguros
# na amostra.

teste_renda_seguro <- chisq.test(
  tabela_renda_seguro
)

print(
  teste_renda_seguro
)


# ------------------------------------------------------------
# 10. VERIFICAÇÃO DAS FREQUÊNCIAS ESPERADAS
# ------------------------------------------------------------

# A inspeção das frequências esperadas é necessária para
# avaliar a adequação da aproximação do teste qui-quadrado.

frequencias_esperadas <- teste_renda_seguro$expected

print(
  round(
    frequencias_esperadas,
    2
  )
)

sum(
  frequencias_esperadas < 5
)

mean(
  frequencias_esperadas < 5
) * 100


# ------------------------------------------------------------
# 11. RESÍDUOS PADRONIZADOS
# ------------------------------------------------------------

# Os resíduos padronizados auxiliam na identificação das
# células que mais contribuem para a associação observada.
#
# Esta etapa é complementar ao teste global.

residuos_padronizados <-
  teste_renda_seguro$stdres

print(
  round(
    residuos_padronizados,
    2
  )
)


# ------------------------------------------------------------
# 12. GRÁFICO DA DISTRIBUIÇÃO
# ------------------------------------------------------------

grafico_renda_seguro <- ggplot(
  tabela_renda_seguro_df,
  aes(
    x = Renda,
    y = Percentual,
    fill = Consumo
  )
) +
  geom_col(
    position = "stack"
  ) +
  geom_text(
    aes(
      label = ifelse(
        Percentual > 0,
        paste0(
          round(Percentual, 1),
          "%"
        ),
        ""
      )
    ),
    position = position_stack(
      vjust = 0.5
    ),
    size = 3
  ) +
  coord_flip() +
  labs(
    title = "Consumo de Seguros por Faixa de Renda",
    subtitle = paste0(
      "Distribuição percentual por faixa de renda | n = ",
      n_validos
    ),
    x = "Renda per capita familiar",
    y = "Percentual de Respondentes (%)",
    fill = "Família tem ou já teve seguro",
    caption = "Fonte: Elaborado pela autora (2026)."
  ) +
  theme_minimal()

grafico_renda_seguro


# ------------------------------------------------------------
# FIM — RENDA × CONSUMO DE SEGUROS
# ------------------------------------------------------------
