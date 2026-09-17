# ============================================================
# ESTADO CIVIL × DECISÃO CONJUNTA
# ============================================================
# Etapa: Análise bivariada / distribuição conjunta
#
# OBJETIVO:
# Analisar como a forma de tomada de decisão relacionada
# à contratação de seguros se distribui segundo o estado
# civil dos respondentes.
#
# A análise é descritiva e considera frequências absolutas
# e percentuais dentro de cada categoria de estado civil.
#
# IMPORTANTE:
# Os resultados descrevem associações observadas na amostra
# e não estabelecem relação causal entre as variáveis.
# ============================================================


# ------------------------------------------------------------
# 1. SELEÇÃO DAS VARIÁVEIS
# ------------------------------------------------------------

df_estado_decisao <- df %>%
  transmute(
    EstadoCivil =
      qual_e_o_estado_civil_do_a_entrevistado_a,

    DecisaoConjunta =
      a_decisao_de_contratar_um_seguro_e_discutida_em_conjunto_pela_familia
  )


# ------------------------------------------------------------
# 2. INSPEÇÃO DOS VALORES ORIGINAIS
# ------------------------------------------------------------

unique(
  df_estado_decisao$EstadoCivil
)

unique(
  df_estado_decisao$DecisaoConjunta
)

table(
  df_estado_decisao$EstadoCivil,
  useNA = "ifany"
)

table(
  df_estado_decisao$DecisaoConjunta,
  useNA = "ifany"
)


# ------------------------------------------------------------
# 3. PADRONIZAÇÃO DO ESTADO CIVIL
# ------------------------------------------------------------

# As categorias são mantidas conforme a estrutura utilizada
# na análise do trabalho.

ordem_estado_civil <- c(
  "Solteiro",
  "Casado",
  "União Estável",
  "Separado Judicialmente"
)

df_estado_decisao <- df_estado_decisao %>%
  mutate(
    EstadoCivil = factor(
      EstadoCivil,
      levels = ordem_estado_civil
    )
  )


# ------------------------------------------------------------
# 4. PADRONIZAÇÃO DA DECISÃO CONJUNTA
# ------------------------------------------------------------

# "Não sei informar" constitui uma resposta válida e,
# portanto, é preservada na análise descritiva.

df_estado_decisao <- df_estado_decisao %>%
  mutate(
    DecisaoConjunta = factor(
      DecisaoConjunta,
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

df_estado_decisao_analise <- df_estado_decisao %>%
  filter(
    !is.na(EstadoCivil),
    !is.na(DecisaoConjunta)
  )

n_validos <- nrow(
  df_estado_decisao_analise
)

n_excluidos <-
  nrow(df_estado_decisao) -
  n_validos

n_validos
n_excluidos


# ------------------------------------------------------------
# 6. TABELA DE CONTINGÊNCIA
# ------------------------------------------------------------

tabela_estado_decisao <- table(
  df_estado_decisao_analise$EstadoCivil,
  df_estado_decisao_analise$DecisaoConjunta
)

print(
  tabela_estado_decisao
)


# ------------------------------------------------------------
# 7. DISTRIBUIÇÃO PERCENTUAL POR ESTADO CIVIL
# ------------------------------------------------------------

# Os percentuais são calculados dentro de cada categoria
# de estado civil.
#
# Assim, cada linha totaliza aproximadamente 100%.

prop_estado_decisao <- prop.table(
  tabela_estado_decisao,
  margin = 1
) * 100

print(
  round(
    prop_estado_decisao,
    2
  )
)


# ------------------------------------------------------------
# 8. TABELA PARA APRESENTAÇÃO
# ------------------------------------------------------------

tabela_estado_decisao_df <- as.data.frame(
  tabela_estado_decisao
) %>%
  rename(
    EstadoCivil = Var1,
    DecisaoConjunta = Var2,
    Frequencia = Freq
  ) %>%
  group_by(
    EstadoCivil
  ) %>%
  mutate(
    Percentual =
      Frequencia /
      sum(Frequencia) * 100
  ) %>%
  ungroup()

print(
  tabela_estado_decisao_df
)


# ------------------------------------------------------------
# 9. VISUALIZAÇÃO DA DISTRIBUIÇÃO
# ------------------------------------------------------------

grafico_estado_decisao <- ggplot(
  tabela_estado_decisao_df,
  aes(
    x = EstadoCivil,
    y = Percentual,
    fill = DecisaoConjunta
  )
) +
  geom_col(
    position = "stack"
  ) +
  geom_text(
    aes(
      label = ifelse(
        Frequencia > 0,
        paste0(
          Frequencia,
          " (",
          round(Percentual, 1),
          "%)"
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
    title = "Decisão Conjunta por Estado Civil",
    subtitle = paste0(
      "Distribuição percentual por estado civil | n = ",
      n_validos
    ),
    x = "Estado Civil",
    y = "Percentual de Respondentes (%)",
    fill = "Decisão conjunta",
    caption = "Fonte: Elaborado pela autora (2026)."
  ) +
  theme_minimal()

grafico_estado_decisao


# ------------------------------------------------------------
# FIM — ESTADO CIVIL × DECISÃO CONJUNTA
# ------------------------------------------------------------
