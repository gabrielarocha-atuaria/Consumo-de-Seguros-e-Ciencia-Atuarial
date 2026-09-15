# ============================================================
# V01 — SEXO DO(A) RESPONDENTE
# ============================================================
#
# Etapa: Refinamento do modelo / análise descritiva e exploratória
#
# OBJETIVO:
# Caracterizar a distribuição da amostra segundo o sexo do(a)
# respondente e explorar sua relação com o consumo familiar de seguros.
#
# IMPORTANTE:
# Esta análise integra o refinamento exploratório dos dados.
# Não corresponde ao teste da hipótese principal do TCC, que compara
# o consumo de seguros entre estudantes de Ciências Atuariais e os
# demais cursos do CCSA.
# ============================================================


# ------------------------------------------------------------
# 1. SELEÇÃO DAS VARIÁVEIS
# ------------------------------------------------------------

# Para esta etapa são utilizadas:
# - sexo: variável de caracterização do respondente;
# - TemSeguro_TeveSeguro: variável de consumo familiar de seguros.

df_sexo <- df %>%
  select(sexo, TemSeguro_TeveSeguro)


# ------------------------------------------------------------
# 2. VALIDAÇÃO DA VARIÁVEL V01 — SEXO
# ------------------------------------------------------------

# Sexo é tratado como variável categórica nominal.
# Verificam-se as categorias registradas e a ocorrência de
# respostas ausentes antes da análise.

unique(df_sexo$sexo)

table(df_sexo$sexo, useNA = "always")


# ------------------------------------------------------------
# 3. DISTRIBUIÇÃO DA AMOSTRA
# ------------------------------------------------------------

tabela_sexo <- df_sexo %>%
  tabyl(sexo) %>%
  adorn_pct_formatting(digits = 1) %>%
  adorn_totals("row")

print(tabela_sexo)

# INTERPRETAÇÃO:
# A distribuição entre os respondentes masculinos e femininos
# mostrou-se relativamente equilibrada.
#
# Foi registrada uma observação na categoria "Neutro".
# A resposta é preservada na caracterização descritiva, mas sua
# frequência é insuficiente para constituir um grupo comparável.


# ------------------------------------------------------------
# 4. CRUZAMENTO EXPLORATÓRIO: SEXO × CONSUMO DE SEGUROS
# ------------------------------------------------------------

# As proporções são calculadas dentro de cada categoria de sexo.
# O procedimento permite comparar a distribuição das respostas
# relativas ao consumo familiar de seguros entre os grupos.

tabela_sexo_consumo <- df_sexo %>%
  tabyl(sexo, TemSeguro_TeveSeguro) %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns()

print(tabela_sexo_consumo)


# ------------------------------------------------------------
# 5. DEFINIÇÃO DO RECORTE COMPARÁVEL
# ------------------------------------------------------------

# Para a análise de associação são mantidas as categorias
# Masculino e Feminino.
#
# A observação "Neutro" não é considerada erro nem é eliminada
# da base original. Sua exclusão ocorre somente neste recorte
# comparativo devido à existência de uma única observação (n = 1).

df_sexo_comparavel <- df_sexo %>%
  filter(sexo %in% c("Masculino", "Feminino"))


# ------------------------------------------------------------
# 6. ASSOCIAÇÃO EXPLORATÓRIA
# ------------------------------------------------------------

# O teste Qui-Quadrado de Pearson é utilizado como procedimento
# exploratório para avaliar a associação entre sexo do respondente
# e consumo familiar de seguros.
#
# Este teste não corresponde à hipótese principal do TCC.

tabela_contingencia <- table(
  df_sexo_comparavel$sexo,
  df_sexo_comparavel$TemSeguro_TeveSeguro
)

teste_qui_sexo <- chisq.test(tabela_contingencia)

print(tabela_contingencia)
print(teste_qui_sexo)


# ------------------------------------------------------------
# 7. VISUALIZAÇÃO
# ------------------------------------------------------------

ggplot(
  df_sexo_comparavel,
  aes(x = sexo, fill = TemSeguro_TeveSeguro)
) +
  geom_bar(position = "fill", color = "white") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Consumo de Seguros segundo o Sexo do Respondente",
    subtitle = "Distribuição proporcional dentro de cada grupo",
    x = "Sexo do respondente",
    y = "Proporção",
    fill = "Família tem ou já teve seguro?",
    caption = "Fonte: Elaborado pela autora (2026)."
  ) +
  theme_minimal()


# ------------------------------------------------------------
# FIM — V01 SEXO
# ------------------------------------------------------------
