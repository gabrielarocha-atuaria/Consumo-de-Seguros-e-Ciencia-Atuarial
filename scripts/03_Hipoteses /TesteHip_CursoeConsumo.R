# ============================================================
# TESTE DE HIPÓTESE PRINCIPAL — CURSO × CONSUMO DE SEGUROS
# ============================================================
# Etapa: Teste da hipótese que atendende a questão de pesquisa.
#
# OBJETIVO:
# Comparar a proporção de consumo de seguros entre estudantes
# de Ciências Atuariais e estudantes dos demais cursos
# considerados na análise.
#
# HIPÓTESES:
#
# H0: p_Atuariais <= p_Outros
#
# H1: p_Atuariais > p_Outros
#
# em que p representa a proporção de respondentes cuja
# família tem ou já teve seguro.
#
# O teste é unicaudal, conforme a hipótese de pesquisa.
# ============================================================


# ------------------------------------------------------------
# 1. SELEÇÃO DAS VARIÁVEIS
# ------------------------------------------------------------

df_hipotese <- df %>%
  transmute(
    curso =
      o_a_entrevistado_a_e_estudante_de_que_curso,

    TemSeguro_TeveSeguro =
      alguem_da_familia_pais_e_irmaos_do_a_entrevistado_a_tem_ou_ja_teve_seguro
  )


# ------------------------------------------------------------
# 2. INSPEÇÃO DOS VALORES ORIGINAIS
# ------------------------------------------------------------

unique(df_hipotese$curso)

table(
  df_hipotese$curso,
  useNA = "ifany"
)

table(
  df_hipotese$TemSeguro_TeveSeguro,
  useNA = "ifany"
)


# ------------------------------------------------------------
# 3. HARMONIZAÇÃO DA VARIÁVEL CURSO
# ------------------------------------------------------------

df_hipotese <- df_hipotese %>%
  mutate(
    curso_clean = curso %>%
      toupper() %>%
      iconv(to = "ASCII//TRANSLIT") %>%
      str_trim(),

    curso_final = case_when(
      str_detect(
        curso_clean,
        "CONTABEIS|CONTABILIDADE"
      ) ~ "CIÊNCIAS CONTÁBEIS",

      str_detect(
        curso_clean,
        "ATUARIA|AUTUARIAIS"
      ) ~ "CIÊNCIAS ATUARIAIS",

      str_detect(
        curso_clean,
        "ECONOMICA|ECONOMIA"
      ) ~ "CIÊNCIAS ECONÔMICAS",

      str_detect(
        curso_clean,
        "ADMINISTRACAO|ADM"
      ) ~ "ADMINISTRAÇÃO",

      TRUE ~ "OUTROS"
    )
  )


# ------------------------------------------------------------
# 4. DEFINIÇÃO DOS GRUPOS DA HIPÓTESE
# ------------------------------------------------------------

# Grupo 1:
# estudantes de Ciências Atuariais.
#
# Grupo 2:
# estudantes de Ciências Contábeis,
# Ciências Econômicas e Administração.

cursos_comparacao <- c(
  "CIÊNCIAS ATUARIAIS",
  "CIÊNCIAS CONTÁBEIS",
  "CIÊNCIAS ECONÔMICAS",
  "ADMINISTRAÇÃO"
)

df_teste <- df_hipotese %>%
  filter(
    curso_final %in% cursos_comparacao,
    TemSeguro_TeveSeguro %in% c(
      "Sim",
      "Não"
    )
  ) %>%
  mutate(
    Grupo = if_else(
      curso_final == "CIÊNCIAS ATUARIAIS",
      "Ciências Atuariais",
      "Outros cursos"
    )
  )


# ------------------------------------------------------------
# 5. CONTROLE DAS OBSERVAÇÕES UTILIZADAS
# ------------------------------------------------------------

n_teste <- nrow(df_teste)

n_teste

table(
  df_teste$Grupo
)

table(
  df_teste$TemSeguro_TeveSeguro
)


# ------------------------------------------------------------
# 6. TABELA DE CONTINGÊNCIA
# ------------------------------------------------------------

tabela_grupo_consumo <- table(
  df_teste$Grupo,
  df_teste$TemSeguro_TeveSeguro
)

print(
  tabela_grupo_consumo
)


# ------------------------------------------------------------
# 7. FREQUÊNCIAS E PROPORÇÕES POR GRUPO
# ------------------------------------------------------------

resumo_grupos <- df_teste %>%
  group_by(Grupo) %>%
  summarise(
    Sim = sum(
      TemSeguro_TeveSeguro == "Sim"
    ),

    N = n(),

    Proporcao = Sim / N,

    Percentual = Proporcao * 100,

    .groups = "drop"
  )

print(
  resumo_grupos
)


# ------------------------------------------------------------
# 8. EXTRAÇÃO AUTOMÁTICA DOS DADOS PARA O TESTE
# ------------------------------------------------------------

sucessos_atuaria <- resumo_grupos %>%
  filter(
    Grupo == "Ciências Atuariais"
  ) %>%
  pull(Sim)

n_atuaria <- resumo_grupos %>%
  filter(
    Grupo == "Ciências Atuariais"
  ) %>%
  pull(N)


sucessos_outros <- resumo_grupos %>%
  filter(
    Grupo == "Outros cursos"
  ) %>%
  pull(Sim)

n_outros <- resumo_grupos %>%
  filter(
    Grupo == "Outros cursos"
  ) %>%
  pull(N)


sucessos <- c(
  sucessos_atuaria,
  sucessos_outros
)

totais <- c(
  n_atuaria,
  n_outros
)

sucessos
totais


# ------------------------------------------------------------
# 9. TESTE DE COMPARAÇÃO DE PROPORÇÕES
# ------------------------------------------------------------

# H0: p_Atuariais <= p_Outros
#
# H1: p_Atuariais > p_Outros
#
# alternative = "greater" representa a hipótese
# direcional estabelecida no trabalho.
#
# correct = FALSE mantém o procedimento utilizado
# no script original.

resultado_teste <- prop.test(
  x = sucessos,
  n = totais,
  alternative = "greater",
  correct = FALSE
)

print(
  resultado_teste
)


# ------------------------------------------------------------
# 10. ESTATÍSTICAS DO TESTE
# ------------------------------------------------------------

qui_quadrado <- unname(
  resultado_teste$statistic
)

z_absoluto <- sqrt(
  qui_quadrado
)

p_valor <- resultado_teste$p.value

proporcao_atuaria <-
  sucessos_atuaria /
  n_atuaria

proporcao_outros <-
  sucessos_outros /
  n_outros

diferenca_proporcoes <-
  proporcao_atuaria -
  proporcao_outros


resumo_teste <- tibble(
  Grupo_Atuariais_Sim =
    sucessos_atuaria,

  Grupo_Atuariais_N =
    n_atuaria,

  Proporcao_Atuariais =
    proporcao_atuaria,

  Grupo_Outros_Sim =
    sucessos_outros,

  Grupo_Outros_N =
    n_outros,

  Proporcao_Outros =
    proporcao_outros,

  Diferenca_Proporcoes =
    diferenca_proporcoes,

  Qui_Quadrado =
    qui_quadrado,

  Z_Absoluto =
    z_absoluto,

  P_Valor =
    p_valor
)

print(
  resumo_teste
)


# ------------------------------------------------------------
# 11. VISUALIZAÇÃO DAS PROPORÇÕES
# ------------------------------------------------------------

grafico_hipotese <- resumo_grupos %>%
  ggplot(
    aes(
      x = Grupo,
      y = Percentual
    )
  ) +
  geom_col(
    width = 0.65
  ) +
  geom_text(
    aes(
      label = paste0(
        Sim,
        "/",
        N,
        " (",
        round(Percentual, 1),
        "%)"
      )
    ),
    vjust = -0.5,
    size = 3.5
  ) +
  scale_y_continuous(
    limits = c(0, 100),
    expand = expansion(
      mult = c(0, 0.08)
    )
  ) +
  labs(
    title = "Consumo de Seguros por Grupo Acadêmico",
    subtitle =
      "Ciências Atuariais versus demais cursos considerados na comparação",
    x = NULL,
    y = "Respondentes com experiência familiar com seguro (%)",
    caption = "Fonte: Elaborado pela autora (2026)."
  ) +
  theme_minimal()

grafico_hipotese


# ------------------------------------------------------------
# FIM — TESTE DE HIPÓTESE
# ------------------------------------------------------------
