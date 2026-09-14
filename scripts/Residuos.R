library(ggplot2)
library(dplyr)

# 1. Dados consolidados
dados_grafico_lixo <- data.frame(
  Status = c("Sanitary landfill\n(Adequate)", 
             "Open dump\n(Inadequate)", 
             "Inert waste only\n(Masked dump)", 
             "Undeclared / null\n(Data omission)"),
  Count = c(2, 8, 2, 8)  # Total = 20 municípios
)

# 2. Calcular porcentagens
dados_grafico_lixo <- dados_grafico_lixo %>%
  mutate(Percentage = (Count / sum(Count)) * 100,
         Status = factor(Status, levels = c("Sanitary landfill\n(Adequate)", 
                                            "Inert waste only\n(Masked dump)", 
                                            "Open dump\n(Inadequate)", 
                                            "Undeclared / null\n(Data omission)")))

# 3. Gráfico
ggplot(dados_grafico_lixo, aes(x = Status, y = Count, fill = Status)) +
  geom_bar(stat = "identity", color = "black", width = 0.6) +
  geom_text(aes(label = paste0(Count, " (", round(Percentage, 1), "%)")), 
            vjust = -0.5, family = "serif", size = 5) +
  scale_fill_manual(values = c(
    "Sanitary landfill\n(Adequate)" = "#00326f",      # azul oficial
    "Open dump\n(Inadequate)" = "#e7d159",            # dourado alerta
    "Inert waste only\n(Masked dump)" = "#d95f0e",    # laranja avermelhado
    "Undeclared / null\n(Data omission)" = "#bdbdbd"  # cinza
  )) +
  scale_y_continuous(limits = c(0, 10)) +
  theme_minimal(base_family = "serif") +
  labs(
    title = "Solid waste destination in Northern Minas Gerais (2024)",
    subtitle = "Compliance with the national solid waste policy (PNRS)",
    x = "", 
    y = "Number of municipalities"
  ) +
  theme(
    legend.position = "none",
    text = element_text(family = "serif", size = 12),
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(face = "bold")
  )