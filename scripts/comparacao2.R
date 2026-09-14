library(ggplot2)
library(dplyr)

# 1. Dados consolidados da nossa extração sobre o Tipo de Sistema de Drenagem
dados_grafico_drenagem <- data.frame(
  Sistema = c("Non-existent\n(High Vulnerability)", "Data Omission\n(Null / Unreported)", 
              "Combined / Unitary\n(Inadequate)", "Separation System\n(Adequate)"),
  Quantidade = c(8, 4, 4, 4) # Total = 20 municípios
)

# 2. Calcular as percentagens e ordenar as categorias
dados_grafico_drenagem <- dados_grafico_drenagem %>%
  mutate(
    Percentagem = (Quantidade / sum(Quantidade)) * 100,
    Sistema = factor(Sistema, levels = c("Non-existent\n(High Vulnerability)", 
                                         "Data Omission\n(Null / Unreported)", 
                                         "Combined / Unitary\n(Inadequate)", 
                                         "Separation System\n(Adequate)"))
  )

# 3. Gerar o Gráfico de Barras
ggplot(dados_grafico_drenagem, aes(x = Sistema, y = Quantidade, fill = Sistema)) +
  geom_bar(stat = "identity", color = "black", width = 0.6) +
  # Adicionar os rótulos com os valores e percentagens no topo de cada barra
  geom_text(aes(label = paste0(Quantidade, " (", Percentagem, "%)")), vjust = -0.5, family = "serif", size = 5) +
  # Paleta de cores focada no alerta visual
  scale_fill_manual(values = c(
    "Separation System\n(Adequate)" = "#00326f",       # Azul oficial
    "Combined / Unitary\n(Inadequate)" = "#e7d159",    # Dourado de alerta
    "Non-existent\n(High Vulnerability)" = "#d95f0e",  # Laranja/Vermelho (Perigo)
    "Data Omission\n(Null / Unreported)" = "#bdbdbd"   # Cinzento (Invisibilidade estatística)
  )) +
  scale_y_continuous(limits = c(0, 10)) +
  theme_minimal(base_family = "serif") +
  labs(
    title = "Stormwater Drainage Systems in Northern Minas Gerais (2024)",
    subtitle = "Physical Infrastructure and Vulnerability to Flooding",
    x = "", 
    y = "Number of Municipalities"
  ) +
  theme(
    legend.position = "none", # Sem necessidade de legenda lateral
    text = element_text(family = "serif", size = 11),
    plot.title = element_text(face = "bold", size = 13),
    axis.text.x = element_text(face = "bold")
  )