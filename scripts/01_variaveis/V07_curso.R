# ============================================================
# V07 — CURSO DO(A) RESPONDENTE
# ============================================================
# Etapa: Tratamento textual / análise descritiva
#
# OBJETIVO:
# Padronizar as informações referentes ao curso dos respondentes
# e descrever a composição acadêmica da amostra.
#
# IMPORTANTE:
# A variável curso também é utilizada posteriormente na hipótese
# principal da pesquisa. Neste script, entretanto, são realizados
# apenas seu tratamento, validação e análise descritiva.
# ============================================================


# ------------------------------------------------------------
# 1. SELEÇÃO DA VARIÁVEL
# ------------------------------------------------------------

df_curso <- df %>%
  select(curso)


# ------------------------------------------------------------
# 2. INSPEÇÃO DAS RESPOSTAS ORIGINAIS
# ------------------------------------------------------------

# A inspeção permite identificar diferentes grafias,
# abreviações e possíveis respostas que não correspondem
# a nomes de cursos.

unique(df_curso$curso)

table(
  df_curso$curso,
  useNA = "ifany"
)


# ------------------------------------------------------------
# 3. DISTRIBUIÇÃO ANTES DA PADRONIZAÇÃO
# ------------------------------------------------------------

tabela_curso_original <- df_curso %>%
  filter(!is.na(curso)) %>%
  count(
    curso,
    name = "Frequencia"
  ) %>%
  mutate(
    Porcentagem =
      Frequencia / sum(Frequencia) * 100
  ) %>%
  arrange(desc(Frequencia))

print(tabela_curso_original)


# ------------------------------------------------------------
# 4. NORMALIZAÇÃO TEXTUAL
# ------------------------------------------------------------

# É criada uma variável auxiliar para tornar comparáveis
# diferentes formas de escrita.
#
# A normalização:
# - remove espaços excedentes;
# - converte o texto para caixa alta;
# - remove diferenças de acentuação.
#
# A resposta original permanece preservada em "curso".

df_curso <- df_curso %>%
  mutate(
    curso_clean = curso %>%
      stringr::str_trim() %>%
      stringr::str_to_upper() %>%
      iconv(
        from = "UTF-8",
        to = "ASCII//TRANSLIT"
      )
  )


# ------------------------------------------------------------
# 5. PADRONIZAÇÃO DOS PRINCIPAIS CURSOS
# ------------------------------------------------------------

# Diferentes formas de registrar o mesmo curso são reunidas
# em uma categoria padronizada.
#
# As regras abaixo correspondem às variações identificadas
# durante o tratamento original da base.

df_curso <- df_curso %>%
  mutate(
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

      TRUE ~ curso_clean
    )
  )


# ------------------------------------------------------------
# 6. IDENTIFICAÇÃO DE RESPOSTAS INVÁLIDAS
# ------------------------------------------------------------

# Respostas que não representam cursos são classificadas
# como ausentes na variável tratada.
#
# A resposta original continua disponível em "curso",
# permitindo auditoria das decisões de tratamento.

df_curso <- df_curso %>%
  mutate(
    curso_final = case_when(

      curso_final %in% c(
        "SIM",
        "NAO",
        "NÃO",
        "NÃO INFORMADO"
      ) ~ NA_character_,

      TRUE ~ curso_final
    )
  )


# ------------------------------------------------------------
# 7. CONFERÊNCIA DA PADRONIZAÇÃO
# ------------------------------------------------------------

# A tabela relaciona cada resposta original à categoria
# atribuída após o tratamento.

tabela_conferencia_curso <- df_curso %>%
  count(
    curso,
    curso_clean,
    curso_final,
    sort = TRUE
  )

print(
  tabela_conferencia_curso,
  n = Inf
)


# ------------------------------------------------------------
# 8. CONTROLE DE OBSERVAÇÕES
# ------------------------------------------------------------

n_validos_curso <- sum(
  !is.na(df_curso$curso_final)
)

n_ausentes_curso <- sum(
  is.na(df_curso$curso_final)
)

n_validos_curso
n_ausentes_curso


# ------------------------------------------------------------
# 9. DISTRIBUIÇÃO CONSOLIDADA POR CURSO
# ------------------------------------------------------------

tabela_curso <- df_curso %>%
  filter(
    !is.na(curso_final)
  ) %>%
  count(
    curso_final,
    name = "Frequencia"
  ) %>%
  mutate(
    Porcentagem =
      Frequencia / sum(Frequencia) * 100
  ) %>%
  arrange(desc(Frequencia))

print(tabela_curso)


# ------------------------------------------------------------
# 10. VISUALIZAÇÃO DA DISTRIBUIÇÃO
# ------------------------------------------------------------

grafico_curso <- ggplot(
  tabela_curso,
  aes(
    x = reorder(
      curso_final,
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
    size = 3.5,
    fontface = "bold"
  ) +
  coord_flip() +
  scale_y_continuous(
    limits = c(
      0,
      max(tabela_curso$Frequencia) * 1.2
    )
  ) +
  labs(
    title = "Distribuição da Amostra por Curso",
    subtitle = paste0(
      "Total de respondentes válidos: n = ",
      sum(tabela_curso$Frequencia)
    ),
    x = "Curso",
    y = "Frequência Absoluta (n)",
    caption = "Fonte: Elaborado pela autora (2026)."
  ) +
  theme_minimal()

grafico_curso


# ------------------------------------------------------------
# 11. AGRUPAMENTO DOS CURSOS DO CCSA
# ------------------------------------------------------------

# É criada uma classificação auxiliar que distingue os quatro
# cursos do CCSA considerados na análise comparativa.
#
# Outros cursos permanecem identificados separadamente como
# "OUTROS". Essa classificação não altera "curso_final".

df_curso <- df_curso %>%
  mutate(
    curso_ccsa = case_when(

      curso_final %in% c(
        "CIÊNCIAS CONTÁBEIS",
        "CIÊNCIAS ATUARIAIS",
        "CIÊNCIAS ECONÔMICAS",
        "ADMINISTRAÇÃO"
      ) ~ curso_final,

      !is.na(curso_final) ~ "OUTROS",

      TRUE ~ NA_character_
    )
  )


# ------------------------------------------------------------
# 12. DISTRIBUIÇÃO DOS CURSOS DO CCSA E OUTROS
# ------------------------------------------------------------

tabela_curso_ccsa <- df_curso %>%
  filter(
    !is.na(curso_ccsa)
  ) %>%
  count(
    curso_ccsa,
    name = "Frequencia"
  ) %>%
  mutate(
    Porcentagem =
      Frequencia / sum(Frequencia) * 100
  ) %>%
  arrange(desc(Frequencia))

print(tabela_curso_ccsa)


# ------------------------------------------------------------
# FIM — V07 CURSO
# ------------------------------------------------------------
