# ==============================================================================
# FIGURA 2: IMPACTOS HUMANOS - DRENAGEM (CORES PERSONALIZADAS)
# ==============================================================================
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)

# 1. Caminho da planilha
caminho_indicadores <- "C:/Users/.../SINISA_AGUASPLUVIAIS_Indicadores_AP2024.xlsx"

# 2. Lendo os dados
dados_ind <- read_excel(caminho_indicadores, skip = 10)

# 3. Consertando cabeçalho
colnames(dados_ind)[2] <- "Nome_Mun"
colnames(dados_ind)[3] <- "UF"

# 4. Cidades do artigo
cidades_artigo <- c("Montes Claros", "Bocaiúva", "Bonito de Minas", "Capitão Enéas", 
                    "Claro dos Poções", "Cônego Marinho", "Coração de Jesus", 
                    "Glaucilândia", "Januária", "Juramento", "Juvenília", 
                    "Lontra", "Manga", "Mirabela", "Miravânia", 
                    "Montalvânia", "São João da Lagoa", "São João do Pacuí", 
                    "Francisco Sá", "Patis")

# 5. Extraindo e limpando os dados
resumo_desastres <- dados_ind %>%
  filter(Nome_Mun %in% cidades_artigo & UF == "MG") %>%
  select(
    Municipality = Nome_Mun,
    Metric1 = IGR0002, # Taxa de Desalojados
    Metric2 = IGR0004  # Taxa de Óbitos/Impactados
  ) %>%
  mutate(across(starts_with("Metric"), ~suppressWarnings(as.numeric(.))))

# 6. Pivotar para longo e traduzir labels
dados_grafico_long <- resumo_desastres %>%
  pivot_longer(cols = starts_with("Metric"), names_to = "Metric_Type", values_to = "Value") %>%
  mutate(Metric_Type = case_when(
    Metric_Type == "Metric1" ~ "Displaced or homeless persons (%)",
    Metric_Type == "Metric2" ~ "Impacted persons, deaths or injuries (rate per 100k hab.)"
  ))

# 7. Ordenação (Polo no topo)
ordem_cidades <- resumo_desastres %>%
  arrange(desc(Municipality == "Montes Claros"), desc(Metric2)) %>%
  pull(Municipality) %>%
  rev()

dados_grafico_long$Municipality <- factor(dados_grafico_long$Municipality, levels = ordem_cidades)

# -----------------------------------------------------------------------------
# 8. CRIANDO O GRÁFICO COM A SUA PALETA (#00326f e #e7d159)
# -----------------------------------------------------------------------------
grafico_impactos <- ggplot(dados_grafico_long, aes(x = Value, y = Municipality)) +
  # Barras com as suas cores personalizadas
  geom_col(aes(fill = Metric_Type), color = "black", linewidth = 0.1, show.legend = FALSE, width = 0.7) +
  
  # AQUI ESTÁ A MUDANÇA: Escala Manual com seus Hex Codes
  scale_fill_manual(values = c("Displaced or homeless persons (%)" = "#e7d159", 
                               "Impacted persons, deaths or injuries (rate per 100k hab.)" = "#00326f")) +
  
  facet_wrap(~Metric_Type, scales = "free_x", strip.position = "bottom") +
  
  # Texto para NAs
  geom_text(data = subset(dados_grafico_long, is.na(Value)),
            aes(x = 0, label = "No data"), 
            hjust = -0.1, color = "grey50", size = 3, family = "serif", fontface = "italic") +
  
  theme_minimal(base_size = 12, base_family = "serif") + 
  theme(
    text = element_text(family = "serif"),
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5, margin = margin(b = 10)),
    axis.text.y = element_text(face = ifelse(levels(dados_grafico_long$Municipality) == "Montes Claros", "bold", "plain"), size = 9),
    strip.text = element_text(face = "bold", size = 10, margin = margin(t = 10)),
    axis.title.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(2, "lines")
  ) +
  labs(title = "Human impact of hydro‑meteorological disasters (2024)",
       subtitle = "Comparative matrix between regional hub and peripheral municipalities",
       y = "")

# 9. Exibir e Salvar
print(grafico_impactos)

setwd("C:/Users/.../Artigo_GRSU/")
ggsave("Figure2_Drainage_Impacts_CustomColors.tiff", 
       plot = grafico_impactos, width = 12, height = 8, dpi = 300, compression = "lzw")

message("Feito! As cores foram atualizadas para Azul Marinho e Dourado.")