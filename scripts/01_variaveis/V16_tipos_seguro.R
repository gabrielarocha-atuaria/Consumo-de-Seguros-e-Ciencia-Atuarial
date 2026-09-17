# ============================================================
# V16 — TIPOS DE SEGURO
# ============================================================
# Etapa: Tratamento de respostas múltiplas / análise descritiva
#
# OBJETIVO:
# Identificar e descrever os tipos de seguro que as famílias
# dos respondentes têm ou já tiveram.
#
# IMPORTANTE:
# A pergunta permite múltiplas respostas. Portanto, cada tipo
# identificado é representado por uma variável indicadora:
#
# 1 = tipo de seguro identificado
# 0 = tipo de seguro não identificado
#
# Como um respondente pode informar mais de um tipo de seguro,
# a soma dos percentuais pode ultrapassar 100%.
# ============================================================


# ------------------------------------------------------------
# 1. SELEÇÃO DA VARIÁVEL
# ------------------------------------------------------------

df_tipos <- df %>%
  transmute(
    id_respondente = row_number(),
    TiposdeSeguro =
      que_tipo_de_seguro_a_sua_familia_tem_ou_ja_teve
  )


# ------------------------------------------------------------
# 2. INSPEÇÃO DAS RESPOSTAS ORIGINAIS
# ------------------------------------------------------------

head(
  df_tipos$TiposdeSeguro
)

unique(
  df_tipos$TiposdeSeguro
)

table(
  is.na(df_tipos$TiposdeSeguro)
)


# ------------------------------------------------------------
# 3. LIMPEZA DO TEXTO
# ------------------------------------------------------------

df_tipos <- df_tipos %>%
  mutate(
    TiposdeSeguro_Limpo =
      stringr::str_squish(TiposdeSeguro)
  )


# ------------------------------------------------------------
# 4. CONTROLE DE OBSERVAÇÕES
# ------------------------------------------------------------

n_validos_tipos <- sum(
  !is.na(df_tipos$TiposdeSeguro_Limpo) &
    df_tipos$TiposdeSeguro_Limpo != ""
)

n_ausentes_tipos <- sum(
  is.na(df_tipos$TiposdeSeguro_Limpo) |
    df_tipos$TiposdeSeguro_Limpo == ""
)

n_validos_tipos
n_ausentes_tipos


# ------------------------------------------------------------
# 5. CRIAÇÃO DAS VARIÁVEIS INDICADORAS
# ------------------------------------------------------------

# As categorias abaixo seguem os padrões textuais identificados
# no tratamento original da variável.

df_tipos_dummy <- df_tipos %>%
  mutate(

    seguro_danos = as.integer(
      stringr::str_detect(
        TiposdeSeguro_Limpo,
        stringr::regex(
          "Danos",
          ignore_case = TRUE
        )
      )
    ),

    seguro_vida = as.integer(
      stringr::str_detect(
        TiposdeSeguro_Limpo,
        stringr::regex(
          "Pessoas|vida",
          ignore_case = TRUE
        )
      )
    ),

    seguro_previdencia = as.integer(
      stringr::str_detect(
        TiposdeSeguro_Limpo,
        stringr::regex(
          "PGBL|previdência",
          ignore_case = TRUE
        )
      )
    ),

    seguro_viagem = as.integer(
      stringr::str_detect(
        TiposdeSeguro_Limpo,
        stringr::regex(
          "Viagem|Viagens",
          ignore_case = TRUE
        )
      )
    ),

    seguro_financeiro = as.integer(
      stringr::str_detect(
        TiposdeSeguro_Limpo,
        stringr::regex(
          "Financeiros",
          ignore_case = TRUE
        )
      )
    ),

    seguro_capitalizacao = as.integer(
      stringr::str_detect(
        TiposdeSeguro_Limpo,
        stringr::regex(
          "Capitalização",
          ignore_case = TRUE
        )
      )
    ),

    # Consolida as variações relacionadas a veículos
    # utilizadas no tratamento original.

    seguro_veiculo = as.integer(
      stringr::str_detect(
        TiposdeSeguro_Limpo,
        stringr::regex(
          "Veículo|Veicular|moto",
          ignore_case = TRUE
        )
      )
    ),

    # Identifica respostas que indicam ausência de seguro.

    seguro_nenhum = as.integer(
      stringr::str_detect(
        TiposdeSeguro_Limpo,
        stringr::regex(
          "Não|Nunca|Nenhum",
          ignore_case = TRUE
        )
      )
    )
  )


# ------------------------------------------------------------
# 6. TRATAMENTO DAS RESPOSTAS AUSENTES
# ------------------------------------------------------------

# As respostas ausentes são mantidas como ausentes.
# Não são automaticamente interpretadas como ausência de seguro.

df_tipos_dummy <- df_tipos_dummy %>%
  mutate(
    across(
      starts_with("seguro_"),
      ~ ifelse(
        is.na(TiposdeSeguro_Limpo),
        NA_integer_,
        .
      )
    )
  )


# ------------------------------------------------------------
# 7. CONFERÊNCIA DAS DUMMIES
# ------------------------------------------------------------

df_tipos_dummy %>%
  select(
    TiposdeSeguro,
    starts_with("seguro_")
  ) %>%
  head(20)


# ------------------------------------------------------------
# 8. FREQUÊNCIA DOS TIPOS DE SEGURO
# ------------------------------------------------------------

resumo_tipos <- df_tipos_dummy %>%
  filter(
    !is.na(TiposdeSeguro_Limpo),
    TiposdeSeguro_Limpo != ""
  ) %>%
  select(
    starts_with("seguro_")
  ) %>%
  summarise(
    across(
      everything(),
      ~ sum(.x, na.rm = TRUE)
    )
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Tipo_de_Seguro",
    values_to = "Frequencia"
  ) %>%
  mutate(
    Tipo_de_Seguro =
      stringr::str_remove(
        Tipo_de_Seguro,
        "^seguro_"
      ),

    Tipo_de_Seguro =
      dplyr::recode(
        Tipo_de_Seguro,
        "danos" = "Danos",
        "vida" = "Pessoas / Vida",
        "previdencia" = "Previdência",
        "viagem" = "Viagem",
        "financeiro" = "Financeiro",
        "capitalizacao" = "Capitalização",
        "veiculo" = "Veículo",
        "nenhum" = "Nenhum / Não possui"
      ),

    Percentual =
      Frequencia / n_validos_tipos * 100
  ) %>%
  arrange(
    desc(Frequencia)
  )

print(resumo_tipos)


# ------------------------------------------------------------
# 9. RANKING DOS TIPOS DE SEGURO
# ------------------------------------------------------------

# "Nenhum / Não possui" é preservado na tabela completa,
# mas retirado deste gráfico porque não representa um tipo
# de seguro.

resumo_tipos_seguros <- resumo_tipos %>%
  filter(
    Tipo_de_Seguro != "Nenhum / Não possui"
  )


grafico_tipos <- ggplot(
  resumo_tipos_seguros,
  aes(
    x = reorder(
      Tipo_de_Seguro,
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
    title = "Tipos de Seguro Informados pelas Famílias",
    subtitle = paste0(
      "Respondentes válidos: n = ",
      n_validos_tipos,
      " | Resposta múltipla"
    ),
    x = "Tipo de Seguro",
    y = "Percentual de Respondentes (%)",
    caption = "Fonte: Elaborado pela autora (2026)."
  ) +
  theme_minimal()

grafico_tipos


# ------------------------------------------------------------
# FIM — V16 TIPOS DE SEGURO
# ------------------------------------------------------------
