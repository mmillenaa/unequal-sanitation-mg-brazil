library(ggplot2)
library(dplyr)
library(ggrepel)

# 1. Os dados corrigidos da tabela de água
dados_bolhas <- data.frame(
  Municipality = c("Montes Claros", "Bocaiúva", "Bonito de Minas", "Capitão Enéas", 
                   "Claro dos Poções", "Cônego Marinho", "Coração de Jesus", 
                   "Glaucilândia", "Januária", "Juramento", "Juvenília", 
                   "Lontra", "Manga", "Mirabela", "Miravânia", 
                   "Montalvânia", "São João da Lagoa", "São João do Pacuí", 
                   "Francisco Sá", "Patis"),
  Water_Coverage = c(82.7, 87.1, 36.7, 66.5, 68.8, 37.6, 41.0, 46.6, 64.7, 61.6, 
                     66.3, 61.7, 59.4, 60.2, 48.7, 72.2, 60.6, NA, 67.0, 48.3),
  # Valores de perdas corrigidos conforme o texto e a Figura_4_Oficial
  Distribution_Losses = c(59.6, 48.5, 87.4, 67.5, 76.0, 84.0, NA, 57.0, 77.0, 81.0, 
                          77.0, 91.4, 83.8, 77.0, 80.0, 81.5, 54.0, NA, 68.0, 83.5)
)

# Remover NAs para o gráfico não dar erro
dados_bolhas <- dados_bolhas %>% filter(!is.na(Water_Coverage) & !is.na(Distribution_Losses))

# 2. Criar o Gráfico de Dispersão (Scatter Plot)
p_scatter <- ggplot(dados_bolhas, aes(x = Water_Coverage, y = Distribution_Losses)) +
  
  # Adicionar quadrantes para guiar a leitura (Ajustei o Y para 40, conforme a imagem original)
  geom_vline(xintercept = 60, linetype = "dashed", color = "grey70", linewidth = 1) +
  geom_hline(yintercept = 40, linetype = "dashed", color = "grey70", linewidth = 1) +
  
  # Marcadores simples (pontos fixos) - Atendendo à Major Revision
  geom_point(size = 5, color = "#5c81b9", alpha = 0.9) +
  
  # Nomes dos municípios em cinzento escuro (grey30) 
  geom_text_repel(aes(label = Municipality), 
                  family = "serif", size = 4.5, color = "grey20", fontface = "plain",
                  box.padding = 0.6, point.padding = 0.4, max.overlaps = 20) +
  
  # Anotação sobre os municípios sem dados
  annotate("text", x = 102, y = 31, label = "Sao Joao do Pacui / Coracao de Jesus: no data", 
           family = "serif", size = 3.5, color = "grey50", hjust = 1) +
  
  # Escalas e Temas
  scale_x_continuous(limits = c(25, 105), breaks = seq(30, 100, by = 10)) +
  scale_y_continuous(limits = c(25, 105), breaks = seq(30, 100, by = 10)) +
  theme_minimal(base_family = "serif", base_size = 14) +
  labs(
    title = "Water supply inefficiency in the sample municipalities (2024)",
    subtitle = "Relationship between coverage rate and distribution losses",
    x = "Water coverage rate (%)",
    y = "Distribution losses (%)",
    caption = "Data source: SINISA (2024)"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 13, hjust = 0.5, margin = margin(b = 15)),
    
    # Eixos X e Y sem negrito
    axis.title.x = element_text(face = "plain", margin = margin(t = 10)), 
    axis.title.y = element_text(face = "plain", margin = margin(r = 10)), 
    
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey90"),
    panel.background = element_rect(fill = "#fbfbfb", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

print(p_scatter)

# 3. Guardar com renderização de fonte aprimorada (device = "ragg")
# O pacote ragg (se instalado: install.packages("ragg")) previne erros na renderização de serifas
ggsave("C:/Users/.../Fig4new_Fixed.tiff", 
       plot = p_scatter, width = 10, height = 7, dpi = 1000, compression = "lzw", device = ragg::agg_tiff)

ggsave("C:/Users/.../Fig4new_Fixed.png", 
       plot = p_scatter, width = 10, height = 7, dpi = 1000, device = ragg::agg_png)