library(ggplot2)
library(ggrepel)

# 1. Recriando a tabela 'dados_completos' 
dados_completos <- data.frame(
  municipio = c("Montes Claros", "Bocaiúva", "Bonito de Minas", "Capitão Enéas",
                "Claro dos Poções", "Cônego Marinho", "Coração de Jesus",
                "Francisco Sá", "Glaucilândia", "Januária", "Juramento",
                "Juvenília", "Lontra", "Manga", "Mirabela", "Miravânia",
                "Montalvânia", "Patis", "São João da Lagoa", "São João do Pacuí"),
  
  coleta_perc = c(85.2, 77.9, NA, 48.0, 30.0, NA, 25.0, 68.0, 45.0, 18.9, 
                  55.0, 11.1, 28.0, 19.2, 38.0, NA, 15.0, 22.0, NA, NA),
  
  gap = c(12.0, 11.0, NA, 48.0, 60.0, NA, 65.0, 0.0, 10.0, 75.0, 
          15.0, 85.0, 65.0, 75.0, 60.0, NA, 82.0, 65.0, NA, NA)
)

# 2. Inverter a lógica do Gap para Tratamento e remover NAs
dados_fig6 <- dados_completos[!is.na(dados_completos$coleta_perc) & !is.na(dados_completos$gap), ]
dados_fig6$tratamento_perc <- 100 - dados_fig6$gap

# 3. Criar o Gráfico de Dispersão Combinado
p_fig6 <- ggplot(dados_fig6, aes(x = coleta_perc, y = tratamento_perc)) +
  
  # Linha de meta de universalização (90%) - TEXTO NA HORIZONTAL
  geom_vline(xintercept = 90, linetype = "dashed", color = "gray60", linewidth = 0.8) +
  annotate("text", x = 100, y = 100, label = "Universalisation target (90%)", 
           angle = 0, hjust = 1, size = 4, family = "serif", color = "gray50") +
  
  # Pontos de dispersão
  geom_point(size = 4, color = "#00326f", alpha = 0.8) +
  
  # ggrepel ajustado: mais força de repulsão e margens maiores para as caixas
  geom_text_repel(aes(label = municipio), 
                  family = "serif", size = 4.2, color = "grey20", fontface = "bold",
                  box.padding = 0.9, point.padding = 0.5, force = 8, 
                  max.overlaps = Inf, min.segment.length = 0) +
  
  # Nota explícita sobre a invisibilidade estatística
  annotate("text", x = 102, y = 12, 
           label = "Statistical invisibility (no data):\nBonito de Minas, Cônego Marinho,\nMiravânia, São João da Lagoa\nand São João do Pacuí", 
           hjust = 1, vjust = 0, family = "serif", size = 3.5, color = "#00326f", fontface = "italic") +
  
  # Escalas fixas expandidas até 105 para não cortar os rótulos nas bordas
  scale_x_continuous(limits = c(0, 105), breaks = seq(0, 100, 20)) +
  scale_y_continuous(limits = c(0, 105), breaks = seq(0, 100, 20)) +
  
  labs(
    title = "Sanitation indicators in northern Minas Gerais (2024)",
    subtitle = "Sewage collection coverage vs. Treated sewage",
    x = "Sewage collection coverage (% of urban population)",
    y = "Treated sewage (% of collected volume)",
    caption = "Data source: SINISA (2024)"
  ) +
  
  theme_minimal(base_family = "serif", base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, margin = margin(b = 10)),
    axis.title.x = element_text(face = "plain", margin = margin(t = 10)), 
    axis.title.y = element_text(face = "plain", margin = margin(r = 10)), 
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey90"),
    panel.background = element_rect(fill = "#f9f9f9", color = NA)
  )

print(p_fig6)

# 4. Salvar imagens (usando ragg para blindar a renderização da serifa)
ggsave("C:/Users/.../Fig6new_Positive_Fixed.tiff", 
       plot = p_fig6, width = 10, height = 7, dpi = 1000, compression = "lzw", bg = "white", device = ragg::agg_tiff)

ggsave("C:/Users/.../Fig6new_Positive_Fixed.png", 
       plot = p_fig6, width = 10, height = 7, dpi = 1000, bg = "white", device = ragg::agg_png)