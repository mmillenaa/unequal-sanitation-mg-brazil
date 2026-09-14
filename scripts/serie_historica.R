library(ggplot2)
library(dplyr)
library(patchwork)

# ==========================================
# 1. CARREGANDO OS DADOS DO GRÁFICO A
# ==========================================
# Criei esta tabela baseada na descrição do seu texto apenas para o código rodar.
# (Substitua esta parte pelo seu comando de importação real, ex: read_excel(...))
evolucao_residuos <- data.frame(
  Ano_Referencia = rep(2017:2022, each = 3),
  Destinacao_Status = rep(c("Adequado (Aterro Sanitário)", 
                            "Inadequado (Aterro Controlado)", 
                            "Inadequado (Lixão)"), times = 6),
  n = c(2, 5, 12,   # 2017
        2, 6, 11,   # 2018
        2, 6, 11,   # 2019
        3, 7, 9,    # 2020
        3, 10, 6,   # 2021 (Pico de aterros controlados)
        4, 5, 10)   # 2022 (Aterro atinge 20%, lixão 50%)
)

# Renomeando a coluna para o padrão que estava no seu código original
colnames(evolucao_residuos)[1] <- "Ano de Referência"

# Calcular percentuais
evolucao_residuos <- evolucao_residuos %>%
  group_by(`Ano de Referência`) %>%
  mutate(total = sum(n),
         percent = n / total * 100) %>%
  ungroup()

# Traduzir os nomes das categorias para inglês (britânico)
evolucao_residuos <- evolucao_residuos %>%
  mutate(Destinacao_Status_eng = case_when(
    Destinacao_Status == "Adequado (Aterro Sanitário)" ~ "Adequate (Sanitary landfill)",
    Destinacao_Status == "Inadequado (Aterro Controlado)" ~ "Inadequate (Controlled landfill)",
    Destinacao_Status == "Inadequado (Lixão)" ~ "Inadequate (Open dump)",
    TRUE ~ Destinacao_Status
  ))

# Cores da paleta
cores_paleta <- c(
  "Adequate (Sanitary landfill)" = "#00326f",
  "Inadequate (Controlled landfill)" = "#e7d159",
  "Inadequate (Open dump)" = "#D95F0E"
)

# Salvar o Gráfico A
grafico_A <- ggplot(evolucao_residuos, 
                    aes(x = `Ano de Referência`, y = percent, 
                        color = Destinacao_Status_eng, group = Destinacao_Status_eng)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_color_manual(values = cores_paleta) +
  scale_y_continuous(labels = scales::percent_format(scale = 1), 
                     limits = c(0, 100)) +
  labs(
    title = "A. Historical evolution of waste disposal (2017–2022)",
    subtitle = "Peripheral municipalities, northern Minas Gerais",
    x = "Year (SINISA)",
    y = "Percentage of municipalities (%)",
    color = "Disposal status"
  ) +
  theme_minimal(base_family = "serif") +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(angle = 0, hjust = 0.5)
  )


# ==========================================
# 2. CARREGANDO OS DADOS E CRIANDO GRÁFICO B
# ==========================================
dados_grafico_lixo <- data.frame(
  Status = c("Sanitary landfill\n(Adequate)", 
             "Open dump\n(Inadequate)", 
             "Inert waste only\n(Masked dump)", 
             "Undeclared / null\n(Data omission)"),
  Count = c(2, 8, 2, 8)
)

dados_grafico_lixo <- dados_grafico_lixo %>%
  mutate(Percentage = (Count / sum(Count)) * 100,
         Status = factor(Status, levels = c("Sanitary landfill\n(Adequate)", 
                                            "Inert waste only\n(Masked dump)", 
                                            "Open dump\n(Inadequate)", 
                                            "Undeclared / null\n(Data omission)")))

grafico_B <- ggplot(dados_grafico_lixo, aes(x = Status, y = Count, fill = Status)) +
  geom_bar(stat = "identity", color = "black", width = 0.6) +
  geom_text(aes(label = paste0(Count, " (", round(Percentage, 1), "%)")), 
            vjust = -0.5, family = "serif", size = 5) +
  scale_fill_manual(values = c(
    "Sanitary landfill\n(Adequate)" = "#00326f",
    "Open dump\n(Inadequate)" = "#e7d159",
    "Inert waste only\n(Masked dump)" = "#d95f0e",
    "Undeclared / null\n(Data omission)" = "#bdbdbd"
  )) +
  scale_y_continuous(limits = c(0, 10)) +
  theme_minimal(base_family = "serif") +
  labs(
    title = "B. Solid waste destination in Northern Minas Gerais (2024)",
    subtitle = "Compliance with the national solid waste policy",
    x = "", 
    y = "Number of municipalities"
  ) +
  theme(
    legend.position = "none",
    text = element_text(family = "serif", size = 12),
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(face = "bold")
  )


# ==========================================
# 3. COMBINAR OS GRÁFICOS (UM EM CIMA DO OUTRO)
# ==========================================
# O pacote patchwork usa a barra "/" para empilhar verticalmente
grafico_final <- grafico_A / grafico_B

# Exibir o resultado final
grafico_final