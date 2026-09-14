# Carregando os pacotes necessários
library(ggplot2)
library(ggrepel)
library(dplyr)
library(tibble)

# 1. OS DADOS DA TABELA DA VERDADE (Blindados e Corrigidos)
dados_finais <- tribble(
  ~Municipio, ~Cobertura, ~Perda, ~Interrupcao,
  "Bocaiúva", 87.13, 48.65, 14.96,
  "Bonito de Minas", 36.67, 87.45, 8.62,
  "Capitão Enéas", 66.47, 67.56, 8.27,
  "Claro dos Poções", 68.82, 75.89, 6.59,
  "Coração de Jesus", 41.00, NA, NA,
  "Cônego Marinho", 37.55, 83.96, 8.74,
  "Francisco Sá", 67.02, 68.54, 14.95,
  "Glaucilândia", 46.57, 56.96, 6.99,
  "Januária", 64.66, 77.13, 9.91,
  "Juramento", 61.60, 80.53, 7.21,
  "Juvenília", 66.30, 71.46, 9.04,
  "Lontra", 61.69, 91.41, 7.83,
  "Manga", 59.43, 83.79, 10.53,
  "Mirabela", 60.18, 76.80, 8.01,
  "Miravânia", 48.72, 80.09, 9.51,
  "Montalvânia", 72.17, 81.83, 9.27,
  "Montes Claros", 82.70, 59.63, 8.67,
  "Patis", 48.26, 83.41, 7.46,
  "São João da Lagoa", 60.55, 54.16, 2.79
)

# -------------------------------------------------------------------------
# 2. FIGURA 4: BOLHAS AZUIS (Cobertura vs. Perdas)
# -------------------------------------------------------------------------
p_fig4 <- ggplot(filter(dados_finais, !is.na(Perda)), aes(x = Cobertura, y = Perda, size = Cobertura)) +
  geom_point(color = "#4c72b0", alpha = 0.8) +
  geom_text_repel(aes(label = Municipio), 
                  size = 3.5, family = "serif", color = "#333333",
                  box.padding = 0.6, point.padding = 0.5, 
                  max.overlaps = Inf, show.legend = FALSE) +
  geom_vline(xintercept = 60, linetype = "dashed", color = "#bbbbbb", linewidth = 0.8) +
  geom_hline(yintercept = 40, linetype = "dashed", color = "#bbbbbb", linewidth = 0.8) +
  scale_x_continuous(limits = c(30, 100), breaks = seq(30, 100, 10)) + 
  scale_y_continuous(limits = c(30, 100), breaks = seq(30, 100, 10)) +
  scale_size_continuous(range = c(4, 11), name = "Water coverage (%)") +
  # O AJUSTE 1: Nota de dados ausentes inserida na Figura 4
  annotate("text", x = 100, y = 32, label = "Sao Joao do Pacui / Coracao de Jesus: no data", 
           size = 3, family = "serif", color = "gray50", hjust = 1) +
  labs(title = "Water supply inefficiency in the sample municipalities (2024)",
       subtitle = "Relationship between coverage rate and distribution losses",
       x = "Water coverage rate (%)", 
       y = "Distribution losses (%)", 
       caption = "Data source: SINISA (2024)") +
  theme_minimal(base_family = "serif") +
  theme(plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
        plot.subtitle = element_text(size = 10, hjust = 0.5, margin = margin(b=15)),
        legend.position = "bottom", 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "#f9f9f9", color = NA))

print(p_fig4)
ggsave("Figura_4_Oficial.png", plot = p_fig4, width = 8, height = 6, dpi = 300, bg = "white")

# -------------------------------------------------------------------------
# 3. FIGURA 5: PONTOS LARANJAS (Perdas vs. Interrupções)
# -------------------------------------------------------------------------
p_fig5 <- ggplot(filter(dados_finais, !is.na(Interrupcao)), aes(x = Perda, y = Interrupcao)) +
  geom_point(size = 4, color = "#d95f0e", alpha = 0.8) +
  # O AJUSTE 2: Forçando linhas e setas de ligação com o ponto
  geom_text_repel(aes(label = Municipio), 
                  size = 3.5, family = "serif", color = "#333333",
                  box.padding = 0.8, point.padding = 0.4, 
                  max.overlaps = Inf,
                  min.segment.length = 0, # Força a linha a aparecer sempre
                  segment.color = "gray50", # Cor da linha
                  segment.linewidth = 0.6,
                  arrow = arrow(length = unit(0.005, "npc"), type = "closed")) + # Desenha a seta
  geom_vline(xintercept = 40, linetype = "dashed", color = "#bbbbbb", linewidth = 0.8) +
  geom_hline(yintercept = 5, linetype = "dashed", color = "#bbbbbb", linewidth = 0.8) +
  scale_x_continuous(limits = c(30, 100), breaks = seq(30, 100, 10)) + 
  scale_y_continuous(limits = c(0, 20), breaks = seq(0, 20, 5)) +
  # Nota de dados ausentes mantida
  annotate("text", x = 100, y = 19, label = "Sao Joao do Pacui / Coracao de Jesus: no data", 
           size = 3, family = "serif", color = "gray50", hjust = 1) +
  labs(title = "Operational inefficiency in water supply, northern Minas Gerais (2024)",
       x = "Distribution losses (%)", 
       y = "Average interruption duration (hours)", 
       caption = "Data source: SINISA (2024)") +
  theme_minimal(base_family = "serif") +
  theme(plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
        panel.grid.minor = element_blank(), 
        panel.background = element_rect(fill = "#f9f9f9", color = NA))

print(p_fig5)
ggsave("Figura_5_Oficial.png", plot = p_fig5, width = 8, height = 6, dpi = 300, bg = "white")

cat("\nPRONTO! Figuras geradas e salvas com setas e anotações. Procure por 'Figura_4_Oficial.png' e 'Figura_5_Oficial.png'.\n")