library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)

# Caminho correto (substitua se necessário)
data_file <- "C:/Users/.../copy_of_SINISA_GESTAO_MUNICIPAL_Informacoes_2024 (1).xlsx"

# Lista de municípios
municipalities <- c("Montes Claros", "Bocaiúva", "Bonito de Minas", "Capitão Enéas", 
                    "Claro dos Poções", "Cônego Marinho", "Coração de Jesus", 
                    "Glaucilândia", "Januária", "Juramento", "Juvenília", 
                    "Lontra", "Manga", "Mirabela", "Miravânia", 
                    "Montalvânia", "São João da Lagoa", "São João do Pacuí", 
                    "Francisco Sá", "Patis")

# Leitura
raw <- read_excel(data_file, skip = 10)
filtered <- raw %>% filter(Município %in% municipalities & UF == "MG")
selected <- filtered %>% select(Município, contains(c("OGM3004", "OGM2201", "OGM3012")))
colnames(selected) <- c("Municipality", "Sanitation plan", "Waste regulation", "Municipal fund")

# Limpeza dos valores
clean <- selected %>%
  mutate(across(-Municipality, ~ case_when(
    . == "Sim" ~ "Yes",
    . == "Não" ~ "No",
    is.na(.) | . %in% c("null", "NA") ~ "No data",
    TRUE ~ as.character(.)
  )))

# Ordenar municípios por número de "Yes" (do mais capacitado ao menos)
clean <- clean %>%
  mutate(n_yes = rowSums(.[, -1] == "Yes", na.rm = TRUE)) %>%
  arrange(desc(n_yes), Municipality) %>%
  select(-n_yes)

ordered_munis <- clean$Municipality

# Transformar para formato longo
long_data <- clean %>%
  pivot_longer(cols = -Municipality, names_to = "Indicator", values_to = "Status")

long_data$Indicator <- factor(long_data$Indicator, 
                              levels = c("Sanitation plan", "Waste regulation", "Municipal fund"))
long_data$Municipality <- factor(long_data$Municipality, levels = ordered_munis)

# Paleta de cores personalizada
cores_status <- c("Yes" = "#00326f",
                  "No" = "#d95f0e",
                  "No data" = "#e7d159")

# Heatmap com as novas cores
p <- ggplot(long_data, aes(x = Indicator, y = Municipality, fill = Status)) +
  geom_tile(color = "gray30", linewidth = 0.3) +
  scale_fill_manual(values = cores_status, na.value = "#f0f0f0") +
  labs(title = "Institutional capacity for sanitation governance",
       subtitle = "Northern Minas Gerais, 2024",
       x = NULL, y = NULL, fill = "Status",
       caption = "Source: SINISA microdata (2024)") +
  theme_minimal(base_family = "serif", base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, margin = margin(b = 10)),
    plot.caption = element_text(size = 8, hjust = 0, face = "italic"),
    axis.text.x = element_text(angle = 0, hjust = 0.5, face = "bold", size = 10),
    axis.text.y = element_text(size = 9,
                               face = ifelse(levels(long_data$Municipality) == "Montes Claros", "bold", "plain")),
    panel.grid = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 10),
    legend.text = element_text(size = 9),
    plot.margin = margin(10, 10, 10, 10)
  )

print(p)
ggsave("Figure1_Governance_Heatmap_Custom.png", plot = p, width = 10, height = 8, dpi = 300, bg = "white")