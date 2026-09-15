# ============================================================
# V02 — IDADE DO(A) RESPONDENTE
# ============================================================
# Etapa: Refinamento do modelo / análise descritiva e exploratória
#
# OBJETIVO:
# Caracterizar a distribuição etária dos respondentes e explorar
# sua relação com o consumo familiar de seguros.
#
# IMPORTANTE:
# Esta análise integra o refinamento exploratório dos dados.
# Não corresponde ao teste da hipótese principal do TCC.
# ============================================================


# ------------------------------------------------------------
# 1. SELEÇÃO DAS VARIÁVEIS
# ------------------------------------------------------------

# A idade constitui a variável principal desta etapa.
#
# Sexo é utilizado posteriormente na representação da estrutura
# etária da amostra.
#
# A variável de consumo é utilizada no cruzamento exploratório
# entre idade e consumo familiar de seguros.

df_idade <- df %>%
  select(
    idade,
    sexo,
    TemSeguro_TeveSeguro
  )


# ------------------------------------------------------------
# 2. VALIDAÇÃO E TRATAMENTO DA VARIÁVEL V02 — IDADE
# ------------------------------------------------------------

# Idade é tratada como variável quantitativa.
#
# Como as respostas podem ter sido importadas como texto,
# extrai-se a informação numérica antes das análises.
#
# A transformação é realizada programaticamente, preservando
# a rastreabilidade do tratamento aplicado aos dados.

df_idade <- df_idade %>%
  mutate(
    idade = as.numeric(stringr::str_extract(idade, "\\d+"))
  )

# Verificação do tipo da variável após a conversão.

class(df_idade$idade)

# Conferência da distribuição inicial.

summary(df_idade$idade)

# Quantidade de observações válidas e ausentes.

sum(!is.na(df_idade$idade))
sum(is.na(df_idade$idade))


# ------------------------------------------------------------
# 3. DISTRIBUIÇÃO DE FREQUÊNCIAS
# ------------------------------------------------------------

# A frequência absoluta informa o número de respondentes
# registrado em cada idade observada.

freq_abs_idade <- table(df_idade$idade)

print(freq_abs_idade)


# A frequência relativa representa a participação percentual
# de cada idade entre as observações válidas.

freq_rel_idade <- prop.table(freq_abs_idade) * 100

print(freq_rel_idade)


# As frequências absoluta e relativa são reunidas para facilitar
# a inspeção da distribuição.

tabela_idade <- cbind(
  Frequencia = freq_abs_idade,
  Porcentagem = round(freq_rel_idade, 2)
)

print(tabela_idade)


# ------------------------------------------------------------
# 4. ESTATÍSTICAS DESCRITIVAS
# ------------------------------------------------------------

# As medidas de posição e dispersão resumem diferentes
# características da distribuição.
#
# Média e mediana descrevem a posição central dos dados,
# enquanto o desvio-padrão informa sua dispersão.
#
# Mínimo e máximo permitem observar a amplitude dos valores
# registrados, sem classificar previamente valores extremos
# como erros.

media_idade <- mean(df_idade$idade, na.rm = TRUE)

mediana_idade <- median(df_idade$idade, na.rm = TRUE)

desvio_padrao_idade <- sd(df_idade$idade, na.rm = TRUE)

idade_minima <- min(df_idade$idade, na.rm = TRUE)

idade_maxima <- max(df_idade$idade, na.rm = TRUE)

n_idade_valida <- sum(!is.na(df_idade$idade))


resumo_idade <- data.frame(
  Observacoes_validas = n_idade_valida,
  Minimo = idade_minima,
  Maximo = idade_maxima,
  Media = media_idade,
  Mediana = mediana_idade,
  Desvio_padrao = desvio_padrao_idade
)

print(resumo_idade)


# ------------------------------------------------------------
# 5. VISUALIZAÇÃO DA DISTRIBUIÇÃO ETÁRIA
# ------------------------------------------------------------

# O histograma permite observar a forma da distribuição
# da variável quantitativa.
#
# A largura das classes deve permanecer explicitada no código,
# pois diferentes agrupamentos podem alterar a aparência da
# distribuição sem modificar os dados originais.

grafico_distribuicao_idade <- ggplot(
  df_idade,
  aes(x = idade)
) +
  geom_histogram(
    binwidth = 5,
    color = "white"
  ) +
  labs(
    title = "Distribuição das Idades dos Respondentes",
    x = "Idade",
    y = "Frequência",
    caption = "Fonte: Elaborado pela autora (2026)."
  ) +
  theme_minimal()

grafico_distribuicao_idade


# ------------------------------------------------------------
# 6. DISTRIBUIÇÃO DA AMOSTRA POR SEXO E IDADE
# ------------------------------------------------------------

# A pirâmide etária combina sexo e faixas de idade para representar
# a composição demográfica da amostra.
#
# Os valores atribuídos a um dos grupos são convertidos em negativos
# exclusivamente para construir a disposição gráfica em lados opostos.
# Essa transformação é visual e não altera as frequências observadas.


# Definição das faixas etárias.

df_piramide <- df_idade %>%
  filter(
    !is.na(idade),
    sexo %in% c("Masculino", "Feminino")
  ) %>%
  mutate(
    faixa_etaria = cut(
      idade,
      breaks = seq(
        floor(min(idade, na.rm = TRUE) / 5) * 5,
        ceiling(max(idade, na.rm = TRUE) / 5) * 5 + 5,
        by = 5
      ),
      right = FALSE
    )
  ) %>%
  count(faixa_etaria, sexo, name = "n") %>%
  mutate(
    n_plot = ifelse(
      sexo == "Masculino",
      -n,
      n
    )
  )


# Representação gráfica.

grafico_piramide_etaria <- ggplot(
  df_piramide,
  aes(
    x = faixa_etaria,
    y = n_plot,
    fill = sexo
  )
) +
  geom_col(
    width = 0.9,
    color = "white"
  ) +
  geom_hline(
    yintercept = 0
  ) +
  coord_flip() +
  scale_y_continuous(
    labels = abs
  ) +
  labs(
    title = "Pirâmide Etária",
    x = NULL,
    y = "Respondentes",
    fill = "Sexo",
    caption = "Fonte: Elaborado pela autora (2026)."
  ) +
  theme_classic()

grafico_piramide_etaria


# ------------------------------------------------------------
# 7. CRUZAMENTO EXPLORATÓRIO: IDADE × CONSUMO DE SEGUROS
# ------------------------------------------------------------

# Para esta etapa são consideradas observações com informação
# disponível para idade e consumo familiar de seguros.
#
# A remoção de valores ausentes ocorre apenas no recorte necessário
# para esta comparação e não modifica a base original.

df_idade_consumo <- df_idade %>%
  filter(
    !is.na(idade),
    !is.na(TemSeguro_TeveSeguro)
  )


# As estatísticas são calculadas separadamente por categoria de
# consumo para permitir a comparação descritiva entre os grupos.

resumo_idade_consumo <- df_idade_consumo %>%
  group_by(TemSeguro_TeveSeguro) %>%
  summarise(
    n = n(),
    media = mean(idade),
    mediana = median(idade),
    desvio_padrao = sd(idade),
    .groups = "drop"
  )

print(resumo_idade_consumo)


# ------------------------------------------------------------
# 8. VISUALIZAÇÃO: IDADE × CONSUMO DE SEGUROS
# ------------------------------------------------------------

# O boxplot permite comparar a posição, dispersão e distribuição
# da idade entre as categorias de consumo.
#
# A representação gráfica é descritiva e, isoladamente, não permite
# concluir que uma variável cause alterações na outra.

grafico_idade_consumo <- ggplot(
  df_idade_consumo,
  aes(
    x = TemSeguro_TeveSeguro,
    y = idade,
    fill = TemSeguro_TeveSeguro
  )
) +
  geom_boxplot() +
  labs(
    title = "Relação entre Idade e Consumo de Seguro",
    x = "Família tem ou já teve seguro?",
    y = "Idade",
    caption = "Fonte: Elaborado pela autora (2026)."
  ) +
  theme_minimal() +
  theme(
    legend.position = "none"
  )

grafico_idade_consumo


# ------------------------------------------------------------
# 9. COMPARAÇÃO EXPLORATÓRIA ENTRE GRUPOS
# ------------------------------------------------------------

# A análise de variância (ANOVA) é utilizada para avaliar se existem
# evidências de diferenças entre as médias de idade dos grupos
# definidos pela variável de consumo.
#
# O procedimento testa diferenças entre médias e não estabelece
# relação causal entre idade e consumo de seguros.

modelo_anova_idade <- aov(
  idade ~ TemSeguro_TeveSeguro,
  data = df_idade_consumo
)

summary(modelo_anova_idade)


# ------------------------------------------------------------
# 10. COMPARAÇÕES MÚLTIPLAS — TUKEY
# ------------------------------------------------------------

# Quando a análise global indica diferenças entre médias, o
# procedimento de Tukey permite examinar as comparações entre
# pares de grupos.
#
# Os resultados devem ser interpretados em conjunto com as
# características da amostra e com os pressupostos do modelo,
# evitando atribuições causais.

teste_tukey_idade <- TukeyHSD(modelo_anova_idade)

print(teste_tukey_idade)


# ------------------------------------------------------------
# FIM — V02 IDADE
# ------------------------------------------------------------
