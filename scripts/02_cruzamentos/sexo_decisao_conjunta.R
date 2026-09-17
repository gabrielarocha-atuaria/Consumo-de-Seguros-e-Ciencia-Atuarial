# ============================================================
# SEXO × DECISÃO CONJUNTA
# ============================================================
# Etapa: Análise bivariada / associação
#
# OBJETIVO:
# Analisar a distribuição da decisão conjunta relacionada
# à contratação de seguros segundo o sexo dos respondentes
# e verificar a existência de associação entre as variáveis.
#
# IMPORTANTE:
# O teste avalia associação entre as variáveis na amostra.
# Não permite estabelecer relação causal.
# ============================================================


# ------------------------------------------------------------
# 1. SELEÇÃO DAS VARIÁVEIS
# ------------------------------------------------------------

df_sexo_decisao <- df %>%
  transmute(
    Sexo =
      qual_e_o_sexo_do_a_entrevistado_a,

    DecisaoConjunta =
      a_decisao_de_contratar_um_seguro_e_discutida_em_conjunto_pela_familia
  )


# ------------------------------------------------------------
# 2. INSPEÇÃO DOS VALORES ORIGINAIS
# ------------------------------------------------------------

unique(df_sexo_decisao$Sexo)

unique(df_sexo_decisao$DecisaoConjunta)

table(
  df_sexo_decisao$Sexo,
  useNA = "ifany"
)

table(
  df_sexo_decisao$DecisaoConjunta,
  useNA = "ifany"
)


# ------------------------------------------------------------
# 3. PADRONIZAÇÃO DAS VARIÁVEIS
# ------------------------------------------------------------

df_sexo_decisao <- df_sexo_decisao %>%
  mutate(
    Sexo = str_squish(Sexo),
    DecisaoConjunta = str_squish(DecisaoConjunta)
  )


# ------------------------------------------------------------
# 4. ANÁLISE DESCRITIVA COMPLETA
# ------------------------------------------------------------

# Nesta etapa, todas as categorias observadas são preservadas,
# inclusive categorias com frequência reduzida.

tabela_descritiva <- df_sexo_decisao %>%
  filter(
    !is.na(Sexo),
    !is.na(DecisaoConjunta)
  ) %>%
  count(
    Sexo,
    DecisaoConjunta,
    name = "Frequencia"
  ) %>%
  group_by(Sexo) %>%
  mutate(
    Percentual =
      Frequencia /
      sum(Frequencia) * 100
  ) %>%
  ungroup()

print(
  tabela_descritiva
)


# ------------------------------------------------------------
# 5. DEFINIÇÃO DA AMOSTRA PARA O TESTE
# ------------------------------------------------------------

# O teste comparativo considera os grupos com tamanho
# suficiente para a análise inferencial.
#
# Categorias muito reduzidas permanecem documentadas na
# análise descritiva, mas não são incorporadas automaticamente
# à comparação entre os dois grupos principais.

df_teste <- df_sexo_decisao %>%
  filter(
    Sexo %in% c(
      "Masculino",
      "Feminino"
    ),
    DecisaoConjunta %in% c(
      "Sim",
      "Não"
    )
  )


# ------------------------------------------------------------
# 6. CONTROLE DAS OBSERVAÇÕES DO TESTE
# ------------------------------------------------------------

n_teste <- nrow(df_teste)

n_teste

table(
  df_teste$Sexo
)

table(
  df_teste$DecisaoConjunta
)


# ------------------------------------------------------------
# 7. TABELA DE CONTINGÊNCIA
# ------------------------------------------------------------

tabela_sexo_decisao <- table(
  df_teste$Sexo,
  df_teste$DecisaoConjunta
)

print(
  tabela_sexo_decisao
)


# ------------------------------------------------------------
# 8. DISTRIBUIÇÃO PERCENTUAL POR SEXO
# ------------------------------------------------------------

prop_sexo_decisao <- prop.table(
  tabela_sexo_decisao,
  margin = 1
) * 100

print(
  round(
    prop_sexo_decisao,
    2
  )
)


# ------------------------------------------------------------
# 9. TESTE EXATO DE FISHER
# ------------------------------------------------------------

# H0:
# Sexo e decisão conjunta são independentes.
#
# H1:
# Existe associação entre sexo e decisão conjunta.
#
# O teste de Fisher é adequado para avaliar associação
# entre duas variáveis categóricas sem depender da
# aproximação assintótica do teste qui-quadrado.

teste_fisher <- fisher.test(
  tabela_sexo_decisao
)

print(
  teste_fisher
)


# ------------------------------------------------------------
# 10. RESULTADO ORGANIZADO DO TESTE
# ------------------------------------------------------------

resultado_fisher <- tibble(
  Teste = "Teste Exato de Fisher",
  P_Valor = teste_fisher$p.value,
  N = n_teste
)

print(
  resultado_fisher
)


# ------------------------------------------------------------
# 11. TABELA PARA VISUALIZAÇÃO
# ------------------------------------------------------------

tabela_grafico <- as.data.frame(
  tabela_sexo_decisao
) %>%
  rename(
    Sexo = Var1,
    DecisaoConjunta = Var2,
    Frequencia = Freq
  ) %>%
  group_by(Sexo) %>%
  mutate(
    Percentual =
      Frequencia /
      sum(Frequencia) * 100
  ) %>%
  ungroup()

print(
  tabela_grafico
)


# ------------------------------------------------------------
# 12. VISUALIZAÇÃO
# ------------------------------------------------------------

grafico_sexo_decisao <- ggplot(
  tabela_grafico,
  aes(
    x = Sexo,
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
  labs(
    title = "Decisão Conjunta segundo o Sexo",
    subtitle = paste0(
      "Distribuição percentual dos respondentes | n = ",
      n_teste
    ),
    x = "Sexo",
    y = "Percentual de Respondentes (%)",
    fill = "Decisão conjunta",
    caption = "Fonte: Elaborado pela autora (2026)."
  ) +
  theme_minimal()

grafico_sexo_decisao

#Não foi encontrada associação estatisticamente significativa (p = 0,8889). 
Portanto, não se rejeita H0: a amostra não fornece evidência suficiente de associação 
entre sexo e decisão conjunta.

# ------------------------------------------------------------
# FIM — SEXO × DECISÃO CONJUNTA
# ------------------------------------------------------------
